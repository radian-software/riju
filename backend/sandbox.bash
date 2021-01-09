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

function daemon {
    has daemon && eval "$(get daemon)"
}

function setup {
    has setup && eval "$(get setup)"
}

function repl {
    has repl && eval "$(get repl)"
}

function main {
    : > "$(get main)"
    has prefix && get prefix >> "$(get main)"
    get template >> "$(get main)"
    has suffix && get suffix >> "$(get main)"
}

function compile {
    has compile && echo "$(get compile)" && eval "$(get compile)"
}

function run-only {
    has run && echo "$(get run)" && eval "$(get run)"
}

function run {
    compile && run-only
}

function format {
    has format && echo "$(get format.run)" && eval "( $(get format.run) ) < $(get main)"
}

function lsp {
    has lsp.setup && echo "$(get lsp.setup)" && eval "$(get lsp.setup)"
    has lsp && echo "$(get lsp.start)" && eval "$(get lsp.start)"
}

if [[ -z "$NS" ]]; then
    main
    setup
fi
