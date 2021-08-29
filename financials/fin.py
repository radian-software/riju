#!/usr/bin/env python3

import argparse
import collections
import csv
import decimal
import gzip
import io
import json
import logging
import os
import pathlib
import re
import sys
from urllib.parse import urlparse

import boto3

logging.basicConfig(level=logging.INFO)

ROOT = pathlib.Path(__file__).parent


def die(msg):
    raise AssertionError(msg)


def get_csv(year, month, force_download=False):
    target_dir = ROOT / f"{year}-{month:02d}"
    logging.info(f"Using base directory {target_dir}")
    target_dir.mkdir(exist_ok=True)
    latest_csv = target_dir / "latest.csv"
    if force_download or not latest_csv.exists():
        try:
            latest_csv.unlink()
        except FileNotFoundError:
            pass
        s3 = boto3.client("s3")
        o = urlparse(os.environ["BILLING_REPORTS_URL"], allow_fragments=False)
        assert o.scheme == "s3"
        bucket = o.netloc
        base_prefix = o.path.strip("/") + "/"
        report_name = base_prefix.rstrip("/").split("/")[-1]
        logging.info(f"List s3://{bucket}/{base_prefix}")
        month_prefixes = [
            elt["Prefix"]
            for elt in s3.list_objects_v2(
                Bucket=bucket, Prefix=f"{base_prefix}", Delimiter="/"
            )["CommonPrefixes"]
        ]
        if not month_prefixes:
            die("no report prefixes found")
        expected_month_prefix = f"{base_prefix}{year}{month:02d}"
        matching_month_prefixes = [
            p for p in month_prefixes if p.startswith(expected_month_prefix)
        ]
        if not matching_month_prefixes:
            die(f"no report prefix for the specified month ({expected_month_prefix})")
        if len(matching_month_prefixes) > 1:
            die(f"multiple matching report prefixes: {repr(matching_month_prefixes)}")
        (month_prefix,) = matching_month_prefixes
        stream = io.BytesIO()
        manifest_path = f"{month_prefix}{report_name}-Manifest.json"
        logging.info(f"Download s3://{bucket}/{manifest_path} in-memory")
        s3.download_fileobj(bucket, manifest_path, stream)
        manifest = json.loads(stream.getvalue())
        (report_path,) = manifest["reportKeys"]
        if not report_path.endswith(".csv.gz"):
            die(f"unexpected report extension in {report_path}")
        logging.info(f"Get metadata for s3://{bucket}/{report_path}")
        basename = s3.head_object(Bucket=bucket, Key=report_path)[
            "LastModified"
        ].strftime("%Y-%m-%d")
        logging.info(
            f"Download s3://{bucket}/{report_path} to {target_dir.relative_to(ROOT)}/{basename}.csv.gz"
        )
        s3.download_file(bucket, report_path, f"{target_dir}/{basename}.csv.gz")
        logging.info(f"Decompress {basename}.csv.gz")
        with gzip.open(f"{target_dir}/{basename}.csv.gz") as f_read:
            with open(f"{target_dir}/{basename}.csv", "wb") as f_write:
                while chunk := f_read.read(1024):
                    f_write.write(chunk)
        latest_csv.symlink_to(f"{basename}.csv")
    return latest_csv


def read_csv(csv_path):
    rows = []
    with open(csv_path) as f:
        reader = csv.reader(f)
        header = next(reader)
        for row in reader:
            rows.append(dict((key, val) for (key, val) in zip(header, row) if val))
    return rows


def get_tax_key(item):
    service = item["lineItem/ProductCode"]
    usage_type = item["lineItem/UsageType"]
    if "DataTransfer" in usage_type:
        service = "AWSDataTransfer"
    return (service, usage_type)


def embed_taxes(items):
    tax_items = collections.defaultdict(list)
    usage_items = collections.defaultdict(list)
    for item in items:
        item_type = item["lineItem/LineItemType"]
        if item_type == "Tax":
            tax_items[get_tax_key(item)].append(item)
        elif item_type == "Usage":
            usage_items[get_tax_key(item)].append(item)
        else:
            die(f"unexpected line item type {repr(item_type)}")
    for key in tax_items:
        if key not in usage_items:
            die(f"tax for {repr(key)} but no usage for that key")
        tax_cost = sum(item["lineItem/UnblendedCost"] for item in tax_items[key])
        usage_cost = sum(item["lineItem/UnblendedCost"] for item in usage_items[key])
        tax_multiplier = (tax_cost + usage_cost) / usage_cost
        for item in usage_items[key]:
            item["lineItem/UnblendedCost"] *= tax_multiplier
    return [item for group in usage_items.values() for item in group]


def classify_line_item(item, billing_month=None, full=False):
    service = item["lineItem/ProductCode"]
    usage_type = item["lineItem/UsageType"]
    operation = item.get("lineItem/Operation")
    resource = item.get("lineItem/ResourceId")
    project = item.get("resourceTags/user:BillingCategory")
    # In 2021-07, the first month that I was using AWS resources for
    # Riju in a nontrivial capacity, I had subpar billing
    # observability, so a lot of the resources aren't tagged
    # correctly. So for that month specifically, I'm hacking in a
    # couple of heuristics to tag the resources after the fact based
    # on what I know about my usage of AWS.
    if billing_month == "2021-07":
        if resource and "riju" in resource.lower():
            project = "Riju"
        elif resource and "shallan" in resource.lower():
            project = "Shallan"
        elif resource and "veidt" in resource.lower():
            project = "Veidt"
        elif service == "AmazonCloudWatch":
            project = "Riju"
        elif (
            service == "AmazonEC2"
            and resource != "i-077884b74aba86bac"
            and "ElasticIP:IdleAddress" not in usage_type
            and "EBS:SnapshotUsage" not in usage_type
        ):
            project = "Riju"
    # AWS does not let you put tags on a public ECR repository,
    # yippee.
    if service == "AmazonECRPublic" and resource.endswith("repository/riju"):
        project = "Riju"
    category = [
        "Uncategorized",
        service,
        usage_type,
        operation or "(no operation)",
        resource or "(no resource)",
    ]
    if not full:
        if service == "AmazonS3":
            category = ["S3"]
        elif service == "AmazonSNS":
            category = ["SNS"]
        elif service in ("AmazonECR", "AmazonECRPublic"):
            category = ["ECR"]
            if "DataTransfer" in usage_type:
                category.append("Data Transfer")
            elif "TimedStorage" in usage_type:
                category.append("Storage")
            else:
                category.extend(
                    [
                        "Uncategorized",
                        usage_type,
                        operation or "(no operation)",
                        resource or "(no resource)",
                    ]
                )
        elif service == "AmazonEC2":
            category = ["EC2"]
            if "ElasticIP:IdleAddress" in usage_type:
                category.append("EIP")
                # Apparently tags on EIPs are ignored for billing
                # purposes, so we just have to know what we were using
                # them for. (Leaving them uncategorized for 2021-07
                # though.)
                if billing_month != "2021-07":
                    project = "Corona"
            elif "EBS:VolumeUsage" in usage_type:
                category.append("EBS Volume")
                category.extend(["EBS Volume", re.sub(r"^.+\.", "", usage_type)])
            elif "EBS:SnapshotUsage" in usage_type:
                category.append("EBS Snapshot")
            elif (
                "DataTransfer" in usage_type
                or "In-Bytes" in usage_type
                or "Out-Bytes" in usage_type
            ):
                category.append("Data Transfer")
            elif "BoxUsage" in usage_type or "CPUCredits" in usage_type:
                category.extend(["Instance", re.sub(r"^.+:", "", usage_type)])
            else:
                category.extend(
                    [
                        "Uncategorized",
                        usage_type,
                        operation or "(no operation)",
                        resource or "(no resource)",
                    ]
                )
        elif service == "AWSELB":
            category = ["ELB"]
            if "DataTransfer" in usage_type:
                category.append("Data Transfer")
            elif "LCUUsage" in usage_type:
                category.append("LCUs")
            elif "LoadBalancerUsage":
                category.append("Load Balancer")
            else:
                category.extend(
                    [
                        "Uncategorized",
                        usage_type,
                        operation or "(no operation)",
                        resource or "(no resource)",
                    ]
                )
        elif service == "AmazonCloudWatch":
            category = ["CloudWatch"]
        elif service == "awskms":
            category = ["KMS"]
    if not project:
        category.extend(
            [
                usage_type,
                operation or "(no operation)",
                resource or "(no resource)",
            ]
        )
    return [project or "Uncategorized", *category]


def add_to_taxonomy(taxonomy, category, item):
    if category:
        categories = taxonomy.setdefault("categories", {})
        add_to_taxonomy(categories.setdefault(category[0], {}), category[1:], item)
    else:
        taxonomy.setdefault("items", []).append(item)
    taxonomy.setdefault("cost", 0)
    taxonomy["cost"] += float(item["lineItem/UnblendedCost"])


def uncategorized_last(key):
    return (key == "Uncategorized", key)


def print_taxonomy(taxonomy, indent="", file=sys.stdout):
    cost = taxonomy["cost"]
    categories = taxonomy.get("categories", {})
    for category in sorted(categories, key=uncategorized_last):
        subtaxonomy = categories[category]
        cost = subtaxonomy["cost"]
        if cost < 0.01:
            continue
        print(f"{indent}{category} :: ${cost:.2f}", file=file)
        print_taxonomy(subtaxonomy, indent=indent + "  ", file=file)


def classify_costs(csv_path, **kwargs):
    all_items = [item for item in read_csv(csv_path)]
    items = []
    for item in all_items:
        cost = item["lineItem/UnblendedCost"]
        if cost and float(cost):
            items.append({**item, "lineItem/UnblendedCost": float(cost)})
    taxonomy = {}
    for item in embed_taxes(items):
        add_to_taxonomy(taxonomy, ["AWS", *classify_line_item(item, **kwargs)], item)
    return taxonomy


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("date")
    parser.add_argument("-f", "--force-download", action="store_true")
    parser.add_argument("-w", "--write", action="store_true")
    args = parser.parse_args()
    year, month = map(int, args.date.split("-"))
    billing_month = f"{year}-{month:02d}"
    csv_path = get_csv(year, month, force_download=args.force_download)
    taxonomy = classify_costs(csv_path, billing_month=billing_month)
    print_taxonomy(taxonomy)
    if args.write:
        riju_taxonomy = taxonomy["categories"]["AWS"]
        riju_taxonomy["categories"] = {"Riju": riju_taxonomy["categories"]["Riju"]}
        target_dir = ROOT / f"{year}-{month:02d}"
        with open(target_dir / "breakdown.txt", "w") as f:
            print_taxonomy(riju_taxonomy, file=f)


if __name__ == "__main__":
    main()
    sys.exit(0)
