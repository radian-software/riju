#!/usr/bin/env bash

set -euo pipefail

groupadd -g "$(stat -c %g "$PWD")" -o -p '!' -r riju-admin
useradd -u "$(stat -c %u "$PWD")" -g "$(stat -c %g "$PWD")" -o -m -N -l -s /usr/bin/bash -G sudo riju-admin

runuser -u riju-admin -- touch /home/riju-admin/.sudo_as_admin_successful
runuser -u riju-admin -- ln -sT /var/riju/.aws /home/riju-admin/.aws

exec runuser -u riju-admin "$@"
