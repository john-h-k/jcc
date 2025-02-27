#!/usr/bin/env bash

# `jcc.sh [command]`
#     * `format` - format the codebase
#     * `build [mode]` - build the compiler, using debug mode by default
#     * `run [args]` - build, then run the compiler with [args]
#     * `debug [args]` - build, then run the compiler under LLDB with [args]
#     * `test [args]` - build, then run the tests passing [args] to the compiler

build() {
    mode="${1:-Debug}"
    mkdir -p build
    cd build
    if ! (cmake -DCMAKE_BUILD_TYPE=$mode .. && cmake --build .)
    then
        echo "Build failed"
        exit -1
    fi

    cd - > /dev/null
}

debug() {
    build >/dev/null
    jcc=$(readlink -f ./build/jcc)
    cd "$CALLER_DIR"
    lldb -o run -- "$jcc" "$@"
    cd - > /dev/null
}

run() {
    build >/dev/null
    jcc=$(readlink -f ./build/jcc)
    cd "$CALLER_DIR"
    "$jcc" "$@"
    cd - > /dev/null
}

test() {
    build >/dev/null
    ./tests/run.sh "$@"
    exit $?
}

ci-test() {
    build

    JCC_QUIET="1" ./tests/run.sh "$@" -O0 || exit $?
    JCC_QUIET="1" ./tests/run.sh "$@" -O1 || exit $?
    JCC_QUIET="1" ./tests/run.sh "$@" -O2 || exit $?
    JCC_QUIET="1" ./tests/run.sh "$@" -O3 || exit $?
}

cfg() {
    jcc=$(readlink -f ./build/jcc)
    cd "$CALLER_DIR"
    "$jcc" "$@"
    cd - > /dev/null

    for file in $(find $(dirname $0)/build -name '*.gv' -print); do
        name=$(basename $file)
        dot -Tpng "$(dirname $0)/$file" > "$name.png" && open "$name.png"
    done
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

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
_invoke-subcommand "$@"

