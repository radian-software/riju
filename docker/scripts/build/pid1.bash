#!/usr/bin/env bash

set -euo pipefail

groupadd -g "$(stat -c %g "$PWD")" -o -p '!' -r riju
useradd -u "$(stat -c %u "$PWD")" -g "$(stat -c %g "$PWD")" -o -m -N -l -s /usr/bin/bash riju

exec runuser -u riju -g riju "$@"
