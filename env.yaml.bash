#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

registry_password="$(pwgen -s 20 1)"
proxy_password="$(pwgen -s 20 1)"

cat <<EOF
networking:
  domain: riju.example.com  # FIXME

letsEncrypt:
  contactEmail: ops@example.com  # FIXME

registry:
  password: "${registry_password}"
  htpasswd: "$(htpasswd -nbB admin "${registry_password}")"
  httpSecret: "$(pwgen -s 16 1)"
  s3:
    accessKeyId: fixme
    accessKeySecret: fixme
    bucket: fixme
    region: fixme
    endpoint: fixme.digitaloceanspaces.com

proxy:
  password: "${proxy_password}"
  htpasswd: "$(htpasswd -nbB admin "${proxy_password}")"
EOF
