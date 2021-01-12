# This script is sourced by Bash within 'make sandbox'.

if [[ -z "$L" ]]; then
    echo 'environment variable unset: $L' >&2
    exit 1
fi

cfg="$(< "/opt/riju/langs/$L.json")" || exit 1

function get {
    jq -r ".$1" <<< "${cfg}"
}

function has {
    get "$@" | grep -vq '^null$'
}

function riju-exec {
    bash -c "set -euo pipefail; $1"
}

function daemon {
    if has daemon; then
        echo "$(get daemon)"
        riju-exec "$(get daemon)"
    fi
}

function setup {
    if has setup; then
        echo "$(get setup)"
        riju-exec "$(get setup)"
    fi
}

function repl {
    if has repl; then
        echo "$(get repl)"
        riju-exec "$(get repl)"
    fi
}

function main {
    if get main | grep -q /; then
        mkdir -p "$(dirname "$(get main)")"
    fi
    : > "$(get main)"
    has prefix && get prefix >> "$(get main)"
    get template >> "$(get main)"
    has suffix && get suffix >> "$(get main)"
}

function compile {
    if has compile; then
        echo "$(get compile)"
        riju-exec "$(get compile)"
    fi
}

function run-only {
    if has run; then
        echo "$(get run)"
        riju-exec "$(get run)"
    fi
}

function run {
    compile && run-only
}

function format {
    if has format; then
        echo "$(get format.run)"
        riju-exec "( $(get format.run) ) < $(get main)"
    fi
}

function lsp {
    if has lsp.setup; then
        echo "$(get lsp.setup)"
        riju-exec "$(get lsp.setup)"
    fi
    if has lsp; then
        echo "$(get lsp.start)"
        riju-exec "$(get lsp.start)"
    fi
}

if [[ -z "$NS" ]]; then
    main
    setup
fi
