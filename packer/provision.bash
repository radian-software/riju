#!/usr/bin/env bash

set -euo pipefail

: ${ADMIN_PASSWORD}
: ${S3_BUCKET}
: ${SUPERVISOR_ACCESS_TOKEN}

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
}
sudo -E apt-get update
sudo -E apt-get install -y certbot docker-ce docker-ce-cli containerd.io unzip whois

wget -nv https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip -O awscli.zip
unzip -q awscli.zip
sudo ./aws/install

sudo chown root:root /tmp/riju-init-volume /tmp/riju-supervisor /tmp/riju.service
sudo mv /tmp/riju-init-volume /tmp/riju-supervisor /usr/local/bin/
sudo mv /tmp/riju.service /etc/systemd/system/

sudo sed -Ei 's/^#?PermitRootLogin .*/PermitRootLogin no/' /etc/ssh/sshd_config
sudo sed -Ei 's/^#?PasswordAuthentication .*/PasswordAuthentication no/' /etc/ssh/sshd_config
sudo sed -Ei 's/^#?PermitEmptyPasswords .*/PermitEmptyPasswords no/' /etc/ssh/sshd_config
sudo sed -Ei "s/\$AWS_REGION/${AWS_REGION}/" /etc/systemd/system/riju.service
sudo sed -Ei "s/\$S3_BUCKET/${S3_BUCKET}/" /etc/systemd/system/riju.service
sudo sed -Ei "s/\$SUPERVISOR_ACCESS_TOKEN/${SUPERVISOR_ACCESS_TOKEN}/" /etc/systemd/system/riju.service

sudo passwd -l root
sudo useradd admin -g admin -G sudo -s /usr/bin/bash -p "$(echo "${ADMIN_PASSWORD}" | mkpasswd -s)" -m

sudo hostnamectl set-hostname riju

sudo systemctl enable riju

sudo passwd -l ubuntu

popd
rm -rf /tmp/riju-work
