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
  echo  "    <none>      Same as 'run', to allow using this script as CC or similar. Requires the first argument to be a valid file or begin with '-'"
  echo "    help        Show help"
  echo "    configure   Configure build"
  echo "    build       Build JCC"
  echo "    clean       Clean artifacts"
  echo "    clean-all   Clean CMake cache & artifacts"
  echo "    layout      Show project layout"
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

jcc_cmd() {
    read -r cmd

    desc="$1"
    help="$2"

    echo "$cmd"
    case "$cmd" in
        help)
          echo -e "$help"
          exit 1
          ;;
        desc)
          echo -e "$desc"
          exit 1
          ;;
    esac
}

langproc() {
    echo $1 | jcc_cmd \
        '(Temporary) Show output for langproc test' \
        "
JCC langproc
John Kelly <johnharrykelly@gmail.com>

jcc.sh langproc [-h|--help] [-n|--no-mnemonics] FILE

OPTIONS:
    -n, --no-mnemonics 
        Don't print mnemonics (e.g 'ret' instead of 'jalr zero, ra')

FILE:
    The langproc test to run, e.g 'integer/add' or 'integer/add.c'
" || exit 0

         # if ! command -v bat &>/dev/null; then
        #     echo -e "${BOLD}Install 'bat' for syntax highlighting${RESET}"
        # fi

        # echo ""

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

clean-all() {
    # nukes entire build directory
    # occasionally useful

    echo -e "${BOLD}Cleaning..." 1>&2

    mkdir -p build
    rm -rf build/*

    echo -e "${BOLD}Done!\n" 1>&2
}

clean() {
    echo -e "${BOLD}Cleaning..." 1>&2

    cd build
    cmake --build . --target clean
    cd - > /dev/null

    echo -e "${BOLD}Done!\n" 1>&2
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

configure() {
    help() {
        echo "JCC configure"
        echo "John Kelly <johnharrykelly@gmail.com>"
        echo ""
        echo "jcc.sh configure [-h|--help] OPTIONS"
        echo ""
        echo "OPTIONS:"
        echo "    -G "
        echo "        CMake Generator to use (default: 'Ninja' if installed, else 'Unix Makefiles')"
        echo ""
        echo "    --clean "
        echo "        Clean artifacts"
        echo ""
        echo "    --clean-all "
        echo "        Completely clean all artifacts, including CMakeCache"
        echo ""
        echo "    -t, --default-target "
        echo "        Set the default target for the build (equivalent to providing '-target <DEFAULT>' on every invocation)"
        echo ""
        echo "    -m, --mode"
        echo "        Mode to build. Values:"
        echo "            * d | D | deb   | debug          | Debug           - Debug"
        echo "            * r | R | rel   | release        | Release         - Release"
        echo "            * rd| RD| reldeb| relwithdebinfo | RelWithDebInfo  - Release with debug info"
        echo ""
        echo "    --profile-build "
        echo "        Enable '-ftime-trace', output traces to \$BUILD_DIR/traces, and a unified trace to \$BUILD_DIR/trace.json"
        echo ""

        if ! command -v bat &>/dev/null; then
           echo -e "${BOLD}Install 'bat' for syntax highlighting${RESET}"
        fi

        echo ""
    }
    

    mode="Debug"
    default_target=""
    profile_build=""

    # if already configured, use current generator
    generator=$(grep -Eo 'CMAKE_GENERATOR:INTERNAL=.*$' build/CMakeCache.txt | sed 's/^.*=//' 2>/dev/null)

    if [ -z "$generator" ]; then
        # else, default to Ninja, fallback to Make
        generator="$(command -v Ninja &>/dev/null && echo "Ninja" || echo "Unix Makefiles")"
        generator="Unix Makefiles"
    fi

    while [[ $# -gt 0 ]]; do
      case "$1" in
        --help|-h|h|help)
          help
          exit 0
          ;;
        --profile-build)
          profile_build="1"
          shift
          ;;
        -t|--default-target)
          shift
          default_target="$1"
          shift
          ;;
        --clean)
          clean
          shift
          ;;
        --clean-all)
          clean-all
          shift
          ;;
        -m|--mode)
          shift
          mode=$(get_mode "$1")

          if [ -z "$mode" ]; then
            echo -e "${BOLDRED}Unrecognised mode '$1'!${RESET}"
          fi

          shift
          ;;
        -G)
          shift
          generator="$1"
          shift
          ;;
        *)
          echo "Unknown argument: $1"
          exit 1
          ;;
      esac
    done

    echo -e "${BOLD}Build configuration: ${RESET}"
    echo -e "${BOLD}    mode=$mode${RESET}"
    echo -e "${BOLD}    generator=$generator${RESET}"
    if [ -n "$profile_build" ]; then
    echo -e "${BOLD}    profile_build=true"
    fi
    if [ -n "$default_target" ]; then
        echo -e "${BOLD}    default_target=$default_target${RESET}"
    fi
    echo -e ""

    flags=""

    # ninja causes colours to get dropped, so force them
    flags="$flags -fdiagnostics-color=always"

    if [ -n "$profile_build" ]; then
        mkdir -p build/traces
        flags="$flags -ftime-trace=traces"
    fi

    if [ -n "$default_target" ]; then
        # ideally we would statically validate this is a valid target but not sure if there is a way to do that
        # without just hard coding the values here
        echo -e "${BOLD}Building with JCC_DEFAULT_TARGET=$default_target${RESET}" 1>&2

        flags="$flags -DJCC_DEFAULT_TARGET=\"$default_target\""
    fi

    mkdir -p build
    cd build
    if ! (cmake -G "$generator" -DCMAKE_C_FLAGS="$flags" -DCMAKE_BUILD_TYPE=$mode ..); then
        echo -e "${BOLDRED}Configuring build failed!${RESET}"
        exit -1
    fi
}

build() {
    configure "$@"

    num_procs=$(nproc 2> /dev/null || sysctl -n hw.physicalcpu 2> /dev/null || { echo -e "${BOLDYELLOW}Could not find core count; defaulting to 4${RESET}" >&2; echo 4; }; )

    if ! cmake --build . --parallel $num_procs; then
        echo -e "${BOLDRED}Build failed!${RESET}"
        exit -1
    fi

    # we are now in build dir (should make this clearer)
    if [ -d traces ]; then
        jq -s '{traceEvents: map(.traceEvents) | add}' traces/*.json > trace.json
    fi
    
    # if [ -n "$default_target" ]; then
    #     # verifies --default-target
    #     MallocNanoZone=0 ./jcc --version >/dev/null
    # fi

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

# In `debug` and `run`, `MallocNanoZone=0` gets rid of spurious meaningless warnings when address san is turned on on macOS

run() {
    mode=$(get_mode "$1")
    [ -z "$mode" ] && mode="Debug" || shift  
    build --mode "$mode" 1>&2

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

    ./scripts/test.sh "$@" || exit $?
}

test-all() {
    build

    ./scripts/test.sh --arg-group -O0 --arg-group -O1 --arg-group -O2 --arg-group -O3 "$@" || exit $?
}

ci-test() {
    build

    ./scripts/test.sh --quiet --arg-group -O0 --arg-group -O1 --arg-group -O2 --arg-group -O3 "$@" || exit $?
}

cfg() {
    for file in $(find build -name '*.gv' -print); do
        name=$(basename $file)
        dot -Tpng "$file" > "$name.png" && open "$name.png"
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
