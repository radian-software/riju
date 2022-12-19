#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

registry_password="$(pwgen -s 20 1)"

cat <<EOF
networking:
  domain: riju.example.com  # FIXME
  ip: x.y.z.w  # FIXME

contact:
  letsEncryptEmail: ops@example.com  # FIXME

metallb:
  secretkey: "$(pwgen -s 256 1)"

registry:
  password: "${registry_password}"
  htpasswd: "$(htpasswd -nbB admin "${registry_password}")"
EOF
