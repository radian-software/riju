#!/usr/bin/env bash

set -euo pipefail

remote_url="$(git remote get-url origin | sed -E 's|git@github\.com:|https://github.com/|')"
commit="$(git rev-parse HEAD)"

echo >&2 "[ci-run.bash] Fetching build parameters from SSM."

parameters="riju-ci-ami-id riju-docker-repo-host riju-public-docker-repo-host riju-s3-bucket-name"
resp="$(aws ssm get-parameters --names ${parameters})"

read -r ami < <(jq '.Parameters[] | select(.Name == "riju-ci-ami-id").Value' -r <<< "${resp}")
read -r docker_repo < <(jq '.Parameters[] | select(.Name == "riju-docker-repo-host").Value' -r <<< "${resp}")
read -r public_docker_repo < <(jq '.Parameters[] | select(.Name == "riju-public-docker-repo-host").Value' -r <<< "${resp}")
read -r s3_bucket < <(jq '.Parameters[] | select(.Name == "riju-s3-bucket-name").Value' -r <<< "${resp}")

echo >&2 "[ci-run.bash] Launching EC2 instance for CI job."

ebs_config="DeviceName=/dev/sdh,Ebs={DeleteOnTermination=true,VolumeSize=128,VolumeType=gp3}"
instance_tags="ResourceType=instance,Tags=[{Key=Name,Value=Riju CI},{Key=BillingCategory,Value=Riju},{Key=BillingSubcategory,Value=Riju:EC2:CI}]"
ebs_tags="ResourceType=volume,Tags=[{Key=Name,Value=Riju CI},{Key=BillingCategory,Value=Riju},{Key=BillingSubcategory,Value=Riju:EBS:CI}]"

resp="$(aws ec2 run-instances                                      \
            --image-id "${ami}"                                    \
            --instance-type t3.2xlarge                             \
            --security-groups riju-deploy                          \
            --iam-instance-profile Name=riju-deploy                \
            --instance-initiated-shutdown-behavior terminate       \
            --user-data file://tools/ci-user-data.bash             \
            --tag-specifications "${instance_tags}" "${ebs_tags}"  \
            --block-device-mappings "${ebs_config}")"

instance_id="$(jq '.Instances[].InstanceId' -r <<< "${resp}")"

echo >&2 "[ci-run.bash] Waiting for instance ${instance_id} to become ready."

success=
for i in $(seq 1 15); do
    sleep 2
    resp="$(aws ec2 describe-instance-status --instance-id "${instance_id}")"
    status="$(jq '.InstanceStatuses[].InstanceState.Name' -r <<< "${resp}")"
    status="${status:-unknown}"
    case "${status}" in
        pending|unknown)             ;;
        running) success=yes; break  ;;
        *      ) exit 1              ;;
    esac
done

if [[ -z "${success}}" ]]; then
    exit 124
fi

echo >&2 "[ci-run.bash] Waiting for SSH to come online."

success=
for i in $(seq 1 15); do
    if (yes || true) | timeout 5 mssh "ubuntu@${instance_id}" true 2>/dev/null; then
        success=yes
        break
    elif (( $# == 124 )); then
        exit 1
    fi
    sleep 2
done

if [[ -z "${success}}" ]]; then
    exit 124
fi

echo >&2 "[ci-run.bash] Running CI remotely using EC2 Instance Connect."

mssh "ubuntu@${instance_id}" bash <<EOF

set -euo pipefail

sudo riju-init-volume
sudo chown ubuntu:ubuntu /mnt/riju

pushd /mnt/riju

mkdir -p src
pushd src

git clone ${remote_url} riju -b ${commit}
pushd riju

./tools/ci-bootstrap.bash

popd
popd
popd

EOF

echo >&2 "[ci-run.bash] CI completed."
