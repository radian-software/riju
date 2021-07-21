#!/usr/bin/env bash

set -euo pipefail

make ecr
make env CMD="dep deploy:live --registry --publish --yes" Z=xz CI=1 TEST_PATIENCE=4 TEST_CONCURRENCY=1
