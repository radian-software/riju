#!/usr/bin/env bash

set -euo pipefail

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get dist-upgrade

apt-get install -y curl gnupg lsb-release

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -

ubuntu_name="$(lsb_release -cs)"

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb [arch=amd64] https://download.docker.com/linux/ubuntu ${ubuntu_name} stable
EOF

apt-get update
apt-get install docker-ce docker-ce-cli containerd.io

sed -i "s#DOCKER_REPO_REPLACED_BY_PACKER#${DOCKER_REPO}#" /usr/local/bin/riju-deploy

for user in admin deploy; do
    if ! grep -vq "PRIVATE KEY" "/tmp/id_${user}.pub"; then
        echo "${user} public key was set to a private key, aborting" >&2
        exit 1
    fi

    IFS=" " read contents < "/tmp/id_${user}.pub"
    echo "${contents}" > "/tmp/id_${user}.pub"
done

sed -Ei 's/^#?PermitRootLogin .*/PermitRootLogin no/' /etc/ssh/sshd_config
sed -Ei 's/^#?PasswordAuthentication .*/PasswordAuthentication no/' /etc/ssh/sshd_config
sed -Ei 's/^#?PermitEmptyPasswords .*/PermitEmptyPasswords no/' /etc/ssh/sshd_config

passwd -l root
useradd admin -g admin -G sudo -s /usr/bin/bash -p "$(echo "${ADMIN_PASSWORD}" | mkpasswd -s)" -m
useradd deploy -s /usr/bin/bash -p "!"

for user in admin deploy; do
    mkdir -p "/home/${user}/.ssh"
    mv "/tmp/id_${user}.pub" "/home/${user}/.ssh/authorized_keys"
    chown -R "${user}:${user}" "/home/${user}/.ssh"
    chmod -R go-rwx "/home/${user}/.ssh"
done

sed -i 's/^/command="sudo riju-deploy",restrict/' /home/deploy/.ssh/authorized_keys

cat <<"EOF" > /etc/sudoers.d/riju
deploy ALL=(root) NOPASSWD: /usr/local/bin/riju-deploy
EOF
