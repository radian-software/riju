#!/usr/bin/env bash

set -euo pipefail

export AWS_REGION="${AWS_REGION:-$(aws configure get region)}"

if [[ -n "${AWS_REGION}" ]]; then
    echo >&2 "no default AWS region specified, and AWS_REGION unset"
    exit 1
fi

cd packer
packer build config.pkr.hcl
