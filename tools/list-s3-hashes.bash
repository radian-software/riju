#!/usr/bin/env bash

set -euo pipefail

aws s3api list-objects-v2 --bucket riju-debs --prefix hashes | jq -r '.Contents[].Key'
