#!/usr/bin/env bash

set -euo pipefail

tee -a /etc/hosts >/dev/null <<< "127.0.0.1 admin"

groupadd -g "$(stat -c %g "$PWD")" -o -p '!' -r riju
useradd -u "$(stat -c %u "$PWD")" -g "$(stat -c %g "$PWD")" -o -p '!' -m -N -l -s /usr/bin/bash -G sudo riju

runuser -u riju -- touch /home/riju/.sudo_as_admin_successful
runuser -u riju -- ln -sT /var/riju/.aws /home/riju/.aws
runuser -u riju -- ln -sT /var/riju/.docker /home/riju/.docker
runuser -u riju -- ln -sT /var/riju/.ssh /home/riju/.ssh
runuser -u riju -- ln -sT /var/riju/.terraform.d /home/riju/.terraform.d
runuser -u riju -- yarn install

exec runuser -u riju "$@"
