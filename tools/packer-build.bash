#!/usr/bin/env bash

set -euo pipefail

cd packer
packer build config.json
