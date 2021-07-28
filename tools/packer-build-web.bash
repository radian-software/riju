#!/usr/bin/env bash

set -euo pipefail

: ${ADMIN_PASSWORD}
: ${S3_BUCKET}
: ${SUPERVISOR_ACCESS_TOKEN}

export AWS_REGION="${AWS_REGION:-$(aws configure get region)}"

if [[ -z "${AWS_REGION}" ]]; then
    echo >&2 "no default AWS region specified, and AWS_REGION unset"
    exit 1
fi

packer build web.pkr.hcl
