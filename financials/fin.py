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


def classify_line_item(item, full=False):
    service = item["lineItem/ProductCode"]
    usage_type = item["lineItem/UsageType"]
    operation = item.get("lineItem/Operation")
    resource = item.get("lineItem/ResourceId")
    project = item.get("resourceTags/user:BillingCategory")
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
        elif service == "AmazonEC2":
            category = ["EC2"]
        elif service == "AWSELB":
            category = ["ELB"]
        elif service == "AmazonCloudWatch":
            category = ["CloudWatch"]
        elif service == "awskms":
            category = ["KMS"]
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


def print_taxonomy(taxonomy, indent=""):
    categories = taxonomy.get("categories", {})
    for category in sorted(categories, key=uncategorized_last):
        subtaxonomy = categories[category]
        cost = subtaxonomy["cost"]
        print(f"{indent}{category} :: ${cost:.2f}")
        print_taxonomy(subtaxonomy, indent=indent + "  ")


def classify_costs(csv_path, **kwargs):
    items = [item for item in read_csv(csv_path) if item["lineItem/UnblendedCost"]]
    for item in items:
        item["lineItem/UnblendedCost"] = float(item["lineItem/UnblendedCost"])
    taxonomy = {}
    for item in embed_taxes(items):
        add_to_taxonomy(taxonomy, classify_line_item(item, **kwargs), item)
    print_taxonomy(taxonomy)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("date")
    parser.add_argument("-f", "--force-download", action="store_true")
    args = parser.parse_args()
    year, month = map(int, args.date.split("-"))
    csv_path = get_csv(year, month, force_download=args.force_download)
    classify_costs(csv_path)


if __name__ == "__main__":
    main()
    sys.exit(0)
