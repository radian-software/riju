#!/usr/bin/env bash

set -e
set -o pipefail

cp scripts/riju.service /etc/systemd/system/riju.service
cp scripts/riju-serve.bash /usr/local/bin/riju-serve
cp scripts/certbot-pre.bash /etc/letsencrypt/renewal-hooks/pre/riju
cp scripts/certbot-post.bash /etc/letsencrypt/renewal-hooks/post/riju
cp scripts/deploy-phase1.py /usr/local/bin/riju-deploy

systemctl daemon-reload
