#!/usr/bin/env bash

set -euxo pipefail

DOCKER_REPO="${DOCKER_REPO:-DOCKER_REPO_REPLACED_BY_PACKER}"

if [[ -n "${SSH_ORIGINAL_COMMAND}" ]]; then
    set -- ${SSH_ORIGINAL_COMMAND}
fi

function usage {
    echo "usage: rijuctl deploy TAG" >&2
    exit 1
}

function main {
    if (( $# == 0 )); then
        usage
    fi

    subcmd="$1"
    shift
    case "${subcmd}" in
        deploy)
            deploy "$@"
            ;;
        *)
            usage
            ;;
    esac
}

function deploy {
    if (( $# != 1 )); then
        usage
    fi
    tag="$1"
    if [[ -z "${tag}" ]]; then
        usage
    fi

    docker pull "${DOCKER_REPO}:${tag}"
    docker tag riju:live riju:prev
    docker tag "${DOCKER_REPO}:${tag}" riju:live
    docker system prune -f
    systemctl restart riju
}

main "$@"
