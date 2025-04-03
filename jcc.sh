#!/usr/bin/env bash

BOLD="\033[1m"
BOLDRED="\033[1;31m"
BOLDGREEN="\033[1;32m"
BOLDYELLOW="\033[1;33m"
RESET="\033[0m"

help() {
  echo "JCC script helper"
  echo "John Kelly <johnharrykelly@gmail.com>"
  echo ""
  echo "jcc.sh COMMAND"
  echo ""
  echo "COMMANDS:"
  echo "    <none>       Same as 'run', to allow using this script as CC or similar. Requires the first argument to be a valid file or begin with '-'"
  echo "    help         Show help"
  echo "    configure    Configure build"
  echo "    build        Build JCC"
  echo "    clean        Clean artifacts"
  echo "    clean-all    Clean CMake cache & artifacts"
  echo "    layout       Show project layout"
  echo "    run          Build, then run JCC with provided arguments"
  echo "    diff         Build, then run JCC with two different sets of args, and diff them"
  echo "    debug        Build, then run JCC under LLDB/GDB with provided arguments"
  echo "    test         Run tests"
  echo "    test-all     Run tests with all optimisation levels"
  echo "    format       Format codebase"
  echo "    layout       Show code distribution"
  echo "    find-unused  Finds symbols that can be made static"
  echo "    as           Assemble and show output for file"
  echo "    langproc     (Temporary) Show output for langproc test"
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
        echo "    -n, --no-mnemonics "
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

    file=""
    args=()
    while [[ $# -gt 0 ]]; do
      case "$1" in
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
            echo -e "${BOLDRED}Cannot pass multiple files (pass JCC options after '--')${RESET}"
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

    run "$CALLER_DIR$file" "${args[@]}" $flags -S -o - | bat -P --language S
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
    paths=("src/preproc.h src/preproc.c"
           "src/lex.h src/lex.c"
           "src/parse.h src/parse.c src/typechk.h src/typechk.c"
           "src/ir"
           "src/opts"
           "src/lower.c src/lower.h src/graphcol.h src/graphcol.c src/lsra.h src/lsra.c"
           "src/x64.h src/x64.c src/x64 src/rv32i.h src/rv32i.c src/rv32i src/aarch64.h src/aarch64.c src/aarch64 src/eep.h src/eep.c src/eep"
           "src/macOS src/linux"
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

get_mode() {
    case "$1" in
        d|D|deb|debug|Debug)
            echo "Debug"
            ;;
        r|R|rel|release|Release)
            echo "Release"
            ;;
        rd|RD|reldeb|relwithdebinfo|RelWithDebInfo)
            echo "RelWithDebInfo"
            ;;
    esac
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

bs-diff() {
    if ! [[ -f build/jcc0 && -f build/jcc1 ]]; then
        echo "${BOLDRED}Expected jcc0 and jcc1 to be present!${RESET}"
        exit 1
    fi


    cd "$CALLER_DIR"
    "$SCRIPT_DIR"/build/jcc0 "$@" 2>&1 > 0.txt
    "$SCRIPT_DIR"/build/jcc1 "$@" 2>&1 > 1.txt

    delta 0.txt 1.txt

    rm 0.txt
    rm 1.txt

    cd - > /dev/null
}

build() {
    ./scripts/build.sh "$@"
}

# In `debug` and `run`, `MallocNanoZone=0` gets rid of spurious meaningless warnings when address san is turned on on macOS

_run() {
    jcc=$(readlink -f ./build/jcc)
    cd "$CALLER_DIR"
    if [[ $(uname) == "Darwin" ]]; then
        MallocNanoZone=0 "$jcc" "$@"
        exc="$?"
    else
        "$jcc" "$@"
        exc="$?"
    fi

    cd - > /dev/null
    return $exc
}

run() {
    mode=$(get_mode "$1")
    mode_flag=""
    [ -n "$mode" ] && { mode_flag="--mode $mode"; shift; }
    build $mode_flag 1>&2

    _run "$@"
}

profile() {
    case "$1" in
        link|compile|preproc|lex)
            region="$1"
            shift
            ;;
    esac

    build --mode release

    ITER=10
    for ((i = 0; i < $ITER; i++ )); do
        output=$(_run -c -o /dev/null --profile "$@" 2>&1)
        exc="$?"

        if [[ -n "$region " ]]; then
            echo "$output" | grep "$region"
        else
            echo "$output"
        fi

        if ! [[ "$exc" == 0 ]]; then
            echo -e "${BOLDRED}Failed!${RESET}"
            exit 1
        fi
    done
}

debug() {
    mode=$(get_mode "$1")
    [ -z "$mode" ] && mode="Debug" || shift  
    build --mode "$mode" 1>&2

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
        exc="$?"
    else
        $debugger "$jcc" "$@"
        exc="$?"
    fi

    cd - > /dev/null
    return $exc
}

test() {
    build

    ./scripts/test.sh "$@" || exit $?
}

test-all() {
    build

    ./scripts/test.sh --arg-group -O0 --arg-group -O1 --arg-group -O2 --arg-group -O3 "$@" || exit $?
}

ci-test() {
    build

    # temp disable opts tests for quick iteration
    timeout -k 30m 30m ./scripts/test.sh --quiet "$@" || exit $?
    # timeout -k 30m 30m ./scripts/test.sh --quiet --arg-group -O0 --arg-group -O1 --arg-group -O2 --arg-group -O3 "$@" || exit $?
}

cfg() {
    for file in $(find build -name '*.gv' -print); do
        name=$(basename $file)
        dot -Tpng "$file" > "$name.png" && open "$name.png"
    done
}

format() {
    echo "Formatting..."
    fd '.*\.[hc]' src -x clang-format -style=file -i
}

# e.g for `jcc benchmark parse`, `benchmark` is a sub fn
sub_fns=()

_register_sub_fn() {
    sub_fns+=("$1")
}

_invoke-subcommand() {
    local base name func

    base=$0

    if [[ "$fn" == "-h" || "$fn" == "--help" ]]; then
        help
    fi

    unset func
    func="$1"
    shift

    name="$func"

    for sub in "${sub_fns[@]}"; do
        if [ "$sub" = "$func" ]; then
            name="$name $1"
            func="$func-$1"
            shift
            break
        fi
    done

    if [ -z "$func" ]; then
        echo "No subcommand provided; defaulting to 'help' subcommand..."
        func="help"
    fi

    if declare -f "${func}" >/dev/null 2>&1; then
        "${func}" "${@}"
        return $?
    elif [[ $name == "help" || $name == "-h" || $name == "--help" ]]; then
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
            run "$func" "$@"
            return $?
        fi

        matches=( $(compgen -A function | grep "^$func" ) )

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

# handle symlink
if readlink "$0"; then
    SCRIPT_DIR="$(dirname "$(readlink "$0")")"
else
    SCRIPT_DIR="$(dirname "$0")"
fi
    
cd "$SCRIPT_DIR"

source ./scripts/profile.sh
source ./scripts/prereqs.sh
source ./scripts/build.sh
source ./scripts/benchmark.sh

_invoke-subcommand "$@"
