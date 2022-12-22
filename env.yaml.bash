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
  letsEncryptProductionEnabled: false

metallb:
  secretkey: "$(pwgen -s 256 1)"

registry:
  password: "${registry_password}"
  htpasswd: "$(htpasswd -nbB admin "${registry_password}")"

minio:
  accessKey: "$(head -c16 /dev/urandom | xxd -p)"
  secretKey: "$(head -c16 /dev/urandom | xxd -p)"
EOF
