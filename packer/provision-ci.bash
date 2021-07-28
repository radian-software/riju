#!/usr/bin/env bash

set -euo pipefail

# I think there is a race condition related to Ubuntu wanting to do an
# automated system upgrade at boot, which causes 'apt-get update' to
# sometimes fail with an obscure error message.
sleep 5

mkdir /tmp/riju-work
pushd /tmp/riju-work

export DEBIAN_FRONTEND=noninteractive

sudo -E apt-get update
sudo -E apt-get dist-upgrade -y

sudo -E apt-get install -y curl gnupg lsb-release

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo -E apt-key add -

ubuntu_name="$(lsb_release -cs)"

sudo tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb [arch=amd64] https://download.docker.com/linux/ubuntu ${ubuntu_name} stable
EOF

sudo -E apt-get update
sudo -E apt-get install -y docker-ce docker-ce-cli containerd.io make

sudo chown root:root /tmp/riju-init-volume
sudo mv /tmp/riju-init-volume /usr/local/bin/

popd
rm -rf /tmp/riju-work
