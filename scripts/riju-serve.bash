#!/usr/bin/env bash

set -e
set -o pipefail

TLS=1
TLS_PRIVATE_KEY="$(base64 -d /etc/letsencrypt/live/riju.codes/privkey.pem)"
TLS_CERTIFICATE="$(base64 -d /etc/letsencrypt/live/riju.codes/fullchain.pem)"

# Do this separately so that errors in command substitution will crash
# the script.
export TLS TLS_PRIVATE_KEY TLS_CERTIFICATE

docker run --rm -p 0.0.0.0:80:6119 riju:prod
