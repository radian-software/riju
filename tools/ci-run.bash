#!/usr/bin/env bash

set -euo pipefail

echo "${DOCKER_PASSWORD}" | sudo -E docker login --username "${DOCKER_USERNAME}" --password-stdin

make system
make env CMD="dep deploy:live --publish --all" Z=xz CI=1 NI=1 TEST_PATIENCE=4 TEST_CONCURRENCY=1
