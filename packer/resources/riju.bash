#!/usr/bin/env bash

set -e
set -o pipefail

domain="$(ls /etc/letsencrypt/live | grep -v README | head -n1)"

if [[ -n "${domain}" ]]; then
    echo "Detected cert for domain: ${domain}, enabling TLS" >&2
    export TLS=1
    TLS_PRIVATE_KEY="$(base64 "/etc/letsencrypt/live/${domain}/privkey.pem")"
    TLS_CERTIFICATE="$(base64 "/etc/letsencrypt/live/${domain}/fullchain.pem")"
    export TLS_PRIVATE_KEY TLS_CERTIFICATE
    if [[ "${domain}" == riju.codes ]]; then
        echo "Domain is riju.codes, enabling analytics" >&2
        export ANALYTICS=1
    else
        echo "Domain is not riju.codes, disabling analytics" >&2
    fi
else
    echo "No certs installed in /etc/letsencrypt/live, disabling TLS" >&2
fi

if [[ -t 1 ]]; then
    it=-it
else
    it=
fi

docker run ${it} -e TLS -e TLS_PRIVATE_KEY -e TLS_CERTIFICATE -e ANALYTICS \
       --rm -p 0.0.0.0:80:6119 -p 0.0.0.0:443:6120 -h riju riju:live
