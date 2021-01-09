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
    has daemon && riju-exec "$(get daemon)"
}

function setup {
    has setup && riju-exec "$(get setup)"
}

function repl {
    has repl && riju-exec "$(get repl)"
}

function main {
    : > "$(get main)"
    has prefix && get prefix >> "$(get main)"
    get template >> "$(get main)"
    has suffix && get suffix >> "$(get main)"
}

function compile {
    has compile && echo "$(get compile)" && riju-exec "$(get compile)"
}

function run-only {
    has run && echo "$(get run)" && riju-exec "$(get run)"
}

function run {
    compile && run-only
}

function format {
    has format && echo "$(get format.run)" && riju-exec "( $(get format.run) ) < $(get main)"
}

function lsp {
    has lsp.setup && echo "$(get lsp.setup)" && riju-exec "$(get lsp.setup)"
    has lsp && echo "$(get lsp.start)" && riju-exec "$(get lsp.start)"
}

if [[ -z "$NS" ]]; then
    main
    setup
fi
