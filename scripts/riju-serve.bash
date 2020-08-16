#!/usr/bin/env bash

set -e
set -o pipefail

TLS=1
TLS_PRIVATE_KEY="$(base64 /etc/letsencrypt/live/riju.codes/privkey.pem)"
TLS_CERTIFICATE="$(base64 /etc/letsencrypt/live/riju.codes/fullchain.pem)"
ANALYTICS=1

# Do this separately so that errors in command substitution will crash
# the script.
export TLS TLS_PRIVATE_KEY TLS_CERTIFICATE ANALYTICS

if [[ -t 1 ]]; then
    it=-it
else
    it=
fi

docker run ${it} -e TLS -e TLS_PRIVATE_KEY -e TLS_CERTIFICATE -e ANALYTICS \
       --rm -p 0.0.0.0:80:6119 -p 0.0.0.0:443:6120 -h riju riju:prod
