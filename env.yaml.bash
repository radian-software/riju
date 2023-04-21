#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

registry_password="$(pwgen -s 20 1)"
proxy_password="$(pwgen -s 20 1)"

cat <<EOF
networking:
  domain: riju.example.com  # FIXME
  ip: x.y.z.w  # FIXME

s3:
  accessKeyId: fixme
  accessKeySecret: fixme
  bucket: fixme
  region: fixme
  endpoint: fixme.digitaloceanspaces.com

contact:
  letsEncryptEmail: ops@example.com  # FIXME
  letsEncryptProductionEnabled: false

metallb:
  secretkey: "$(pwgen -s 256 1)"

registry:
  password: "${registry_password}"
  htpasswd: "$(htpasswd -nbB admin "${registry_password}")"
  httpSecret: "$(pwgen -s 16 1)"

minio:
  accessKey: "$(head -c16 /dev/urandom | xxd -p)"
  secretKey: "$(head -c16 /dev/urandom | xxd -p)"

proxy:
  password: "${proxy_password}"
  htpasswd: "$(htpasswd -nbB admin "${proxy_password}")"
EOF
