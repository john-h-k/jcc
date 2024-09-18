#!/usr/bin/env bash

build() {
    cd build
    cmake -DCMAKE_BUILD_TYPE=debug -DCMAKE_EXE_LINKER_FLAGS="-fsanitize=undefined" .. \
        && cmake --build .
    cd -
}

test() {
    build
    ./tests/run.sh
}

cfg() {
    cd "$(dirname "$0")"
    "./$(dirname $0)/build/jcc" "$@"
    for file in $(find $(dirname $0)/build -name '*.gv' -print); do
        name=$(basename $file)
        dot -Tpng "$(dirname $0)/$file" > "$name.png" && open "$name.png"
    done
    cd -
}

format() {
    echo "Formatting..."
    fd '.*\.[hc]' . -x clang-format -style=file -i
}

_invoke-subcommand() {
    local base name func

    base=$0

    unset name
    if [ -z "${1}" ]; then
        echo "No subcommand provided; defaulting to 'help' subcommand..."
        name="help"
    fi

    func=${name:="${1}"}

    if declare -f "${func}" >/dev/null 2>&1; then
        shift 1 >/dev/null 2>&1

        "${func}" "${@}"

        if [ $? != 0 ]; then
            return $?
        fi
    elif [[ $name == "help" ]]; then
        func_names=( $(compgen -A function ) )
        func_names=("${func_names[@]/_*}")

        echo "Usage: ${base} <subcommand> [options]"
        echo "Subcommands:"
        for func in "${func_names[@]}"; do
            if ! [[ -z "$func" ]]; then
                echo "    - $base ${func#${base}-}"
            fi
        done
    else
        matches=( $(compgen -A function | grep "^$name" ) )

        if [ "${#matches[@]}" -eq "1" ]; then
            "${matches[0]}" "${@}"
        elif [ "${#matches[@]}" -gt "1" ]; then
            echo "'${name}' is ambiguous; did you mean one of the following?" >&2
            for match in "${matches[@]}"; do
                echo "    - $base ${match#${base}-}" >&2
            done
            return 1
        else
            echo "'${name}' - not valid subcommand for '${base}'" >&2
            return 1
        fi
    fi
}

cd "$(dirname "$0")"
_invoke-subcommand "$@"

