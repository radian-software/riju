#!/usr/bin/env bash

set -euo pipefail

groupadd -g "$(stat -c %g "$PWD")" -o -p '!' -r riju
useradd -u "$(stat -c %u "$PWD")" -g "$(stat -c %g "$PWD")" -o -m -N -l -s /usr/bin/bash -G sudo riju

runuser -u riju -- touch /home/riju/.sudo_as_admin_successful
runuser -u riju -- ln -sT /var/riju/.aws /home/riju/.aws

exec runuser -u riju "$@"
