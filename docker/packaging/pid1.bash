#!/usr/bin/env bash

set -euo pipefail

groupadd -g "$(stat -c %g "$PWD")" -o -p '!' -r riju-packager
useradd -u "$(stat -c %u "$PWD")" -g "$(stat -c %g "$PWD")" -o -m -N -l -s /usr/bin/bash -G sudo riju-packager

runuser -u riju-packager touch /home/riju-packager/.sudo_as_admin_successful

exec runuser -u riju-packager "$@"
