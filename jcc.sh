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
  ech  "    <none>      Same as 'run', to allow using this script as CC or similar. Requires the first argument to be a valid file or begin with '-'"
  echo "    help        Show help"
  echo "    build       Build JCC"
  echo "    run         Build, then run JCC with provided arguments"
  echo "    diff        Build, then run JCC with two different sets of args, and diff them"
  echo "    debug       Build, then run JCC under LLDB/GDB with provided arguments"
  echo "    test        Run tests"
  echo "    test-all    Run tests with all optimisation levels"
  echo "    format      Format codebase"
  echo "    layout      Show code distribution"
  echo "    find-unused Finds symbols that can be made static"
  echo "    as          Assemble and show output for file"
  echo "    langproc    (Temporary) Show output for langproc test"
  echo ""

  exit
}

langproc() {
    help() {
        echo "JCC langproc"
        echo "John Kelly <johnharrykelly@gmail.com>"
        echo ""
        echo "jcc.sh langproc [-h|--help] [-n|--no-mnemonics] FILE"
        echo ""
        echo "OPTIONS:"
        echo "    -n|--no-mnemonics "
        echo "        Don't print mnemonics (e.g 'ret' instead of 'jalr zero, ra')"
        echo ""
        echo "FILE:"
        echo "    The langproc test to run, e.g 'integer/add' or 'integer/add.c'"
        echo ""

        if ! command -v bat &>/dev/null; then
            echo -e "${BOLD}Install 'bat' for syntax highlighting${RESET}"
        fi

        echo ""
    }

    if [[ $# -eq 0 ]]; then
      help
      exit 0
    fi

    file=""
    args=()
    while [[ $# -gt 0 ]]; do
      case "$1" in
        --help|-h|help)
          help
          exit 0
          ;;
        -*)
          args+=("$1")
          shift
          ;;
        *)
          if [ -n "$file" ]; then
            echo -e "${BOLDRED}Cannot pass multiple files${RESET}"
            exit -1
          fi

          file="$1"
          shift
          ;;
        esac
    done

    [[ $file != *.c ]] && file="$file.c"

    dir="$CALLER_DIR"

    if ! [ -f "$CALLER_DIR$file" ]; then
        dir=./tests/langproc

        matches=($(find "$dir" -type f | grep -E "/$file(\.[^.]+)?$"))

        if [[ ${#matches[@]} -eq 1 ]]; then
            file="${matches[0]}"
            dir=""
        elif [[ ${#matches[@]} -gt 1 ]]; then
            echo -e "${BOLDRED}Multiple possible tests for '$file:'"
            printf '%s\n' "${matches[@]}"
            echo -e "${RESET}"
            exit 1
        else
            echo -e "${BOLDRED}Could not find '$file' at '$path'${RESET}"
            exit 1
        fi
    fi

    CALLER_DIR="$dir" as "$file" "${args[@]}" -- -target rv32i-unknown-elf
}

as() {
 help() {
        echo "JCC as"
        echo "John Kelly <johnharrykelly@gmail.com>"
        echo ""
        echo "jcc.sh as [-h|--help] [-n|--no-mnemonics] FILE -- [ARGS]"
        echo ""
        echo "OPTIONS:"
        echo "    -n|--no-mnemonics "
        echo "        Don't print mnemonics (e.g 'ret' instead of 'jalr zero, ra')"
        echo ""
        echo "FILE:"
        echo "    The file to assemble"
        echo ""
        echo "ARGS:"
        echo "    Arguments to pass to JCC"
        echo ""

        if ! command -v bat &>/dev/null; then
            echo -e "${BOLD}Install 'bat' for syntax highlighting${RESET}"
        fi

        echo ""
    }

    if [[ $# -eq 0 ]]; then
      help
      exit 0
    fi

    flags="-C mnemonics"
    args=()

    values=""
    file=""
    while [[ $# -gt 0 ]]; do
      case "$1" in
        --help|-h|help)
          help
          exit 0
          ;;
        -n|--no-mnenomics)
          flags=""
          shift
          ;;
        --)
          values="1"
          shift
          ;;
        *)
          if [[ -n "$values" ]]; then
            args+=("$1")
            shift
            continue
          elif [[ -n "$file" ]]; then
            echo -e "${BOLDRED}Cannot pass multiple files${RESET}"
            exit -1
          fi

          file="$1"
          if ! [[ -f "$CALLER_DIR$file" ]]; then
            echo -e "${BOLDRED}Could not find file '$file'${RESET}"
            exit -1
          fi
          shift
          ;;
        esac
    done

    if ! command -v bat &>/dev/null; then
        bat() {
            cat
        }
    fi

    run "$CALLER_DIR$file" "${args[@]}" $flags -S -o stdout | bat -P --language S
}

find-unused() {
    OBJ_DIR=build/CMakeFiles/jcc.dir

    globals=""
    used=""

    # we look for unused symbols _not in header_
    # which probably should be marked `static`

    for obj_file in "$OBJ_DIR"/**/*.o; do
        [ -e "$obj_file" ] || continue

        source_file="${obj_file#"$OBJ_DIR/"}"
        source_file="${source_file%.o}"
        source_file="${source_file%.c}.h"

        if ! [ -f "$source_file" ]; then
            continue
        fi

        # remove symbols in headers, which are intentionally non-static
        while read -r symbol; do
            unmangled="${symbol#_}"

            if ! grep -qw "$unmangled" "$source_file"; then
                globals+="$symbol"$'\n'
            fi
        done < <(nm -g "$obj_file" 2>/dev/null | awk '{print $3}')

        used+="$(nm -u "$obj_file" 2>/dev/null)"$'\n'
    done

    globals=$(echo "$globals" | sort -u)
    used=$(echo "$used" | sort -u)

    echo -e "${BOLDYELLOW}Unused symbols:${RESET}"
    comm -23 <(echo "$globals") <(echo "$used")
    echo ""
}

layout() {
    count_lines() {
        total=0
        for path in "$@"; do
            if [ -d "$path" ]; then
                dir_count=$(fd -e c -e h . "$path" --exclude .git --exclude build --exec wc -l | awk '{sum += $1} END {print sum}')
                total=$((total + dir_count))
            elif [ -f "$path" ]; then
                file_count=$(wc -l < "$path")
                total=$((total + file_count))
            fi
        done
        echo "$total"
    }

    categories=("preproc" "lex" "parse" "ir" "opts" "lower" "targets" "objects" "tests")
    paths=("preproc.h preproc.c"
           "lex.h lex.c"
           "parse.h parse.c typechk.h typechk.c"
           "ir"
           "opts"
           "lower.c lower.h graphcol.h graphcol.c lsra.h lsra.c"
           "x64.h x64.c x64 rv32i.h rv32i.c rv32i aarch64.h aarch64.c aarch64 eep.h eep.c eep"
           "macOS linux"
           "tests"
           )

    col_width=9

    total_lines=$(count_lines .)

    echo -e "${BOLD}"

    sep="---------"

    printf "| %-*s |" $col_width "total"
    for category in "${categories[@]}"; do
        printf " %-*s |" $col_width "$category"
    done
    printf " %-*s |" $col_width "other"
    printf "\n"

    printf "|-%-*s-" $col_width "$sep"
    for category in "${categories[@]}"; do
        printf "|-%-*s-" $col_width "$sep"
    done
    printf "|-%-*s-" $col_width "$sep"
    printf "|\n"

    printf "| %-*s |" $col_width "$total_lines"
    category_total=0
    for i in "${!categories[@]}"; do
        category_lines=$(count_lines ${paths[$i]})
        category_total=$((category_total + category_lines))
        printf " %-*s |" $col_width "$category_lines"
    done
    other_lines=$((total_lines - category_total))
    printf " %-*s |" $col_width "$other_lines"
    printf "\n"

    printf "|-%-*s-" $col_width "$sep"
    for category in "${categories[@]}"; do
        printf "|-%-*s-" $col_width "$sep"
    done
    printf "|-%-*s-" $col_width "$sep"
    printf "|\n"

    printf "| %-*s |" $col_width "%"
    for i in "${!categories[@]}"; do
        category_lines=$(count_lines ${paths[$i]})
        percentage=$(awk "BEGIN {printf \"%.2f\", ($category_lines/$total_lines)*100}")
        printf " %-*s |" $col_width "$percentage%"
    done
    other_percentage=$(awk "BEGIN {printf \"%.2f\", ($other_lines/$total_lines)*100}")
    printf " %-*s |" $col_width "$other_percentage%"
    printf "\n"

    echo -e "${RESET}"
}


build() {
    mode=$(get_mode "$1")
    mode="${mode:-Debug}"

    args=()

    while [[ $# -gt 0 ]]; do
        if [[ "$1" == "--default-target" ]]; then
            shift
            default_target="$1"
        else
            args+=("$1")
        fi

        shift
    done

    echo -e "${BOLD}Building (mode='$mode')...${RESET}" 1>&2

    flags=""
    if [ -n "$default_target" ]; then
        # ideally we would statically validate this is a valid target but not sure if there is a way to do that
        # without just hard coding the values here
        echo -e "${BOLD}Building with JCC_DEFAULT_TARGET=$default_target${RESET}" 1>&2

        flags="-DJCC_DEFAULT_TARGET=\"$default_target\""
    fi

    mkdir -p build
    cd build
    if ! (cmake -DCMAKE_C_FLAGS="$flags" -DCMAKE_BUILD_TYPE=$mode .. && cmake --build .); then
        echo -e "${BOLDRED}Build failed!${RESET}"
        exit -1
    fi

    echo -e "${BOLD}Build complete${RESET}"  1>&2

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
    delta --diff-args='--histogram' -s $ltmp $rtmp
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

# In `debug` and `run`, `MallocNanoZone=0` gets rid of spurious meaningless warnings when address san is turned on on macOS

run() {
    mode=$(get_mode "$1")
    [ -z "$mode" ] || shift  
    build "$mode"

    jcc=$(readlink -f ./build/jcc)
    cd "$CALLER_DIR"
    if [[ $(uname) == "Darwin" ]]; then
        MallocNanoZone=0 "$jcc" "$@"
    else
        "$jcc" "$@"
    fi

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

    if [[ $(uname) == "Darwin" ]]; then
        MallocNanoZone=0 $debugger "$jcc" "$@"
    else
        $debugger "$jcc" "$@"
    fi

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
        return $?
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
        # sort of hacky feature
        # we want `jcc.sh <args>` to default to `run`
        # we should probably setup a scripts or bin folder to keep them and have a sep script there
        # but for now, do this "auto-run" if the first argument is a flag OR a file
        if [[ -f "$CALLER_DIR$func" || "$func" == -* ]]; then
            run "${@}"
            return $?
        fi

        matches=( $(compgen -A function | grep "^$name" ) )

        if [ "${#matches[@]}" -eq "1" ]; then
            "${matches[0]}" "${@}"
        elif [ "${#matches[@]}" -gt "1" ]; then
            echo "'${name}' is ambiguous; did you mean one of the following?" >&2
            for match in "${matches[@]}"; do
                echo "    - $base ${match#${base}-}" >&2
            done
        else
            echo "'${name}' - not valid subcommand for '${base}'" >&2
        fi

        echo "Note: auto-run feature only works if the first argument to $(basename "$0") is a file or option (begins with '-')"

        return 1
    fi
}

export CALLER_DIR="$(pwd)/"
export CALLER_DIR="${CALLER_DIR%/}/"
cd "$(dirname "$0")"
_invoke-subcommand "$@"

