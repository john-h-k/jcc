#!/usr/bin/env bash

BOLD="\033[1m"
BOLDRED="\033[1;31m"
RESET="\033[0m"

help() {
  echo "JCC script helper"
  echo "John Kelly <johnharrykelly@gmail.com>"
  echo ""
  echo "jcc.sh COMMAND"
  echo ""
  echo "COMMANDS:"
  echo "    help        Show help"
  echo "    run         Build, then run JCC with provided arguments"
  echo "    diff        Build, then run JCC with two different sets of args, and diff them"
  echo "    debug       Build, then run JCC under LLDB/GDB with provided arguments"
  echo "    test        Run tests"
  echo "    test-all    Run tests with all optimisation levels"
  echo "    format      Format codebase"
  echo ""

  exit
}

build() {
    mode=$(get_mode "$1")
    mode="${mode:-Debug}"

    echo -e "${BOLD}Building (mode='$mode')...${RESET}"

    mkdir -p build
    cd build
    if ! (cmake -DCMAKE_BUILD_TYPE=$mode .. && cmake --build .) >/dev/null
    then
        echo -e "${BOLDRED}Build failed!${RESET}"
        exit -1
    fi

    echo -e "${BOLD}Build complete${RESET}"

    cd - > /dev/null
}

diff() {
    l="$1"
    r="$2"
    shift 2

    ltmp=$(mktemp)
    rtmp=$(mktemp)

    trap "rm -f $ltmp $rtmp" EXIT

    echo -e "${BOLD}Running '$@' with args '$l' ${RESET}"
    run $l "$@" &> $ltmp

    echo -e "${BOLD}Running '$@' with args '$r' ${RESET}"
    run $r "$@" &> $rtmp

    # not easy to get columns from within a script :(
    # command diff --width=175 --side-by-side --color=always $ltmp $rtmp
    command sdiff -w 175 $ltmp $rtmp
    rm -f $ltmp $rtmp
}

get_mode() {
    case "$1" in
        d|D|debug|Debug)
            echo "Debug"
            ;;
        r|R|release|Release)
            echo "Release"
            ;;
        rd|RD|reldeb|RelWithDebInfo)
            echo "RelWithDebInfo"
            ;;
    esac
}

# In `debug` and `run`, `MallocNanoZone=0` gets rid of spurious meaningless warnings when address san is turned on

run() {
    mode=$(get_mode "$1")
    [ -z "$mode" ] || shift  
    build "$mode"

    jcc=$(readlink -f ./build/jcc)
    cd "$CALLER_DIR"
    MallocNanoZone=0 "$jcc" "$@"
    cd - > /dev/null
}

debug() {
    mode=$(get_mode "$1")
    [ -z "$mode" ] || shift  
    build "$mode"

    jcc=$(readlink -f ./build/jcc)
    cd "$CALLER_DIR"

    if command -v lldb &>/dev/null; then
        echo -e "${BOLD}Using \`lldb\`"
        debugger="lldb -o run --"
    elif command -v gdb &>/dev/null; then
        echo -e "${BOLD}Using \`gdb\`"
        debugger="gdb -ex run --args"
    else
        echo "${BOLDRED}No debugger found! (tried lldb & gdb)${RESET}"
        exit -1
    fi

    MallocNanoZone=0 $debugger "$jcc" "$@"
    cd - > /dev/null
}

test() {
    build

    ./tests/run.sh "$@" || exit $?
}

test-all() {
    build

    ./tests/run.sh --arg-group -O0 --arg-group -O1 --arg-group -O2 --arg-group -O3 "$@" || exit $?
}

ci-test() {
    build

    ./tests/run.sh --quiet --arg-group -O0 --arg-group -O1 --arg-group -O2 --arg-group -O3 "$@" || exit $?
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

    if [[ "$1" == "-h" || "$1" == "--help" ]]; then
        help
    fi

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

export CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
_invoke-subcommand "$@"

