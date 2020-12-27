#!/usr/bin/env bash

set -euo pipefail

echo "${DOCKER_PASSWORD}" | docker login --username "${DOCKER_USERNAME}" --password-stdin

make publish
