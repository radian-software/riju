#!/usr/bin/env bash

set -euo pipefail

while read -t2 -a cmd; do
    if (( "${#cmd[@]}" > 0 )); then
        case "${cmd[0]}" in
            ping) ;;
            exec|pty)
                if (( "${#cmd[@]}" < 3 )); then
                    echo >&2 "usage: (exec|pty) UUID ARG..."
                else
                    uuid="${cmd[1]}"
                    args=("${cmd[@]:2}")
                    echo >&2 "${cmd[0]} ${args[0]} with UUID ${uuid}"
                    input="/var/run/riju/share/cmd-${uuid}-input"
                    output="/var/run/riju/share/cmd-${uuid}-output"
                    mkfifo "${input}" "${output}"
                    runuser -u riju -- bash -c 'exec "$@"' sentinel "${args[@]}" < "${input}" &> "${output}" &
                fi
                ;;
            *)
                echo >&2 "unrecognized command: ${cmd[0]}"
                ;;
        esac
    fi
done < /var/run/riju/share/control
