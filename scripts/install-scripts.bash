#!/usr/bin/env bash

set -e
set -o pipefail

cp scripts/riju.service /etc/systemd/system/riju.service
cp scripts/certbot-pre.bash /etc/letsencrypt/renewal-hooks/pre/riju
cp scripts/certbot-post.bash /etc/letsencrypt/renewal-hooks/post/riju
cp scripts/install.py /usr/bin/riju-install
chgrp deploy /usr/bin/riju-install
chmod u=rwxs,g=rx,o= /usr/bin/riju-install

systemctl daemon-reload
