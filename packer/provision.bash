#!/usr/bin/env bash

set -euxo pipefail

if [[ -z "${ADMIN_PASSWORD}" ]]; then
    echo "you need to set admin_password in secrets.json" >&2
    exit 1
fi

if [[ -z "${DOCKER_REPO}" ]]; then
    echo "internal error: somehow DOCKER_REPO was not set" >&2
    exit 1
fi

for user in admin deploy; do
    if [[ ! -s "/tmp/id_${user}.pub" ]]; then
        echo "you need to set ${user}_ssh_public_key_file in secrets.json" >&2
        exit 1
    fi

    if ! grep -vq "PRIVATE KEY" "/tmp/id_${user}.pub"; then
        echo "you accidentally set ${user}_ssh_public_key_file to a private key" >&2
        exit 1
    fi

    IFS=" " read contents < "/tmp/id_${user}.pub"
    echo "${contents}" > "/tmp/id_${user}.pub"
done

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get dist-upgrade -y

apt-get install -y apt-transport-https ca-certificates curl gnupg-agent software-properties-common
curl -sSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
add-apt-repository -n universe
add-apt-repository -n "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"

packages="

bsdmainutils
certbot
containerd.io
docker-ce
docker-ce-cli
git
make
members
python3
tmux
vim
whois

"

apt-get update
apt-get install -y ${packages}

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

sed -i 's/^/command="sudo rijuctl",restrict/' /home/deploy/.ssh/authorized_keys

cat <<"EOF" > /etc/sudoers.d/riju
deploy ALL=(root) NOPASSWD: /usr/local/bin/rijuctl
EOF

sed -i "s#DOCKER_REPO_REPLACED_BY_PACKER#${DOCKER_REPO}#" /tmp/rijuctl.bash

mv /tmp/riju.bash /usr/local/bin/riju
mv /tmp/riju.service /etc/systemd/system/riju.service
mv /tmp/rijuctl.bash /usr/local/bin/rijuctl

chmod +x /usr/local/bin/riju
chmod +x /usr/local/bin/rijuctl

rm -rf /var/lib/apt/lists/*
