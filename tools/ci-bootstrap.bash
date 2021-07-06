#!/usr/bin/env bash

set -euo pipefail

: ${AWS_ACCESS_KEY_ID}
: ${AWS_SECRET_ACCESS_KEY}
: ${DOCKER_REPO}
: ${S3_BUCKET}

make image shell I=ci CMD="tools/ci-run.bash" NI=1
