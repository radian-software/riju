#!/usr/bin/env bash

set -euo pipefail

make ecr system
make env CMD="dep deploy:live --publish --all" Z=xz CI=1 TEST_PATIENCE=4 TEST_CONCURRENCY=1
