#!/usr/bin/env bash

set -euo pipefail

cd supervisor
mkdir -p out
go build -o out/riju-supervisor ./src
