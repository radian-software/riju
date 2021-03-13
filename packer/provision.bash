#!/usr/bin/env bash

set -euo pipefail

mkdir /tmp/riju
pushd /tmp/riju

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
sudo -E apt-get install -y certbot docker-ce docker-ce-cli containerd.io unzip whois

wget -nv https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip -O awscli.zip
unzip -q awscli.zip
sudo ./aws/install

sudo chown root:root /tmp/riju /tmp/riju-deploy /tmp/riju.service
sudo mv /tmp/riju /tmp/riju-deploy /tmp/riju-init-volume /tmp/riju-install-certbot-hooks /usr/local/bin/
sudo mv /tmp/riju.service /etc/systemd/system/

for user in admin deploy; do
    if ! grep -vq "PRIVATE KEY" "/tmp/id_${user}.pub"; then
        echo "${user} public key was set to a private key, aborting" >&2
        exit 1
    fi

    IFS=" " read contents < "/tmp/id_${user}.pub"
    echo "${contents}" > "/tmp/id_${user}.pub"
done

sudo sed -Ei 's/^#?PermitRootLogin .*/PermitRootLogin no/' /etc/ssh/sshd_config
sudo sed -Ei 's/^#?PasswordAuthentication .*/PasswordAuthentication no/' /etc/ssh/sshd_config
sudo sed -Ei 's/^#?PermitEmptyPasswords .*/PermitEmptyPasswords no/' /etc/ssh/sshd_config

sudo passwd -l root
sudo useradd admin -g admin -G sudo -s /usr/bin/bash -p "$(echo "${ADMIN_PASSWORD}" | mkpasswd -s)" -m
sudo useradd deploy -s /usr/bin/bash -p "!" -m

for user in admin deploy; do
    sudo runuser -u "${user}" -- mkdir -p "/home/${user}/.ssh"
    sudo mv "/tmp/id_${user}.pub" "/home/${user}/.ssh/authorized_keys"
    sudo chown -R "${user}:${user}" "/home/${user}/.ssh"
    sudo chmod -R go-rwx "/home/${user}/.ssh"
done

sudo runuser -u deploy -- sed -i 's/^/command="sudo riju-deploy ${SSH_ORIGINAL_COMMAND}",restrict /' /home/deploy/.ssh/authorized_keys

sudo tee /etc/sudoers.d/riju >/dev/null <<"EOF"
deploy ALL=(root) NOPASSWD: /usr/local/bin/riju-deploy
EOF

sudo hostnamectl set-hostname riju

sudo systemctl enable riju

sudo passwd -l ubuntu

popd
rm -rf /tmp/riju
