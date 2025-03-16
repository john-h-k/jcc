#!/bin/sh

# This is a standalone - not used by other scripts
# needs lots of logic improvements this is a vEpsilon

# TODO: what should it do if `jcc` folder already exists - unhandled
# TODO: should it delete folder at end? probably yes

BOLD="\033[1m"
BOLDRED="\033[1;31m"
BOLDYELLOW="\033[1;33m"
RESET="\033[0m"

has_tool() {
    command -v "$1" > /dev/null 2>&1
    return $?
}

try_root() {
    has_tool sudo && sudo "$@" || "$@"
}

script_dir="$(dirname "$0")"

ispipe="1"
if [ -f "$script_dir/../jcc.sh" ]; then
    printf "%b\n\n" "${BOLD}Assuming run is from inside JCC directory; will not download${RESET}"

    # use presence of `jcc.sh` as marker for being in repo already (rather than being a piped script)
    ispipe=""

    # hmm, we don't update if in a git repo. maybe this is not optimal
    cd "$script_dir/.."
fi

if [ -n "$ispipe" ]; then
    mkdir -p jcc
    if has_tool git; then
        printf "%b\n" "${BOLD}Downloading with 'git clone'...${RESET}"
        git clone --quiet https://github.com/john-h-k/jcc jcc
    else
        if has_tool curl; then
            printf "%b\n" "${BOLD}Downloading tarball with 'curl'...${RESET}"
            curl -sSL https://github.com/john-h-k/jcc/archive/refs/heads/main.tar.gz | tar xz --strip-components=1 -C jcc
        elif has_tool wget; then
            printf "%b\n" "${BOLD}Downloading tarball with 'wget'...${RESET}"
            wget -q -O - https://github.com/john-h-k/jcc/archive/refs/heads/main.tar.gz | tar xz --strip-components=1 -C jcc
        else
            # could be that the script has been copied over file system or similar
            printf "%b\n" "${BOLDRED}'curl' or 'wget' required to install JCC${RESET}"

            cd - > /dev/null 2>&1
            exit 1
        fi
    fi

    cd jcc
fi

printf "%b\n\n" "${BOLD}Downloading done!${RESET}"

output=""
if has_tool bash && has_tool cmake && [ -z "$JCC_FORCE_SIMPLE_BUILD" ]; then
    output="build/jcc"
    ./jcc.sh build --mode release
else
    printf "%b\n" "${BOLD}Bash/CMake is not installed; reverting to a simple build...$RESET"

    # if bash isn't installed, manually do a simple build (literally feeding all files directly to the C compiler)

    mkdir -p build
    output="build/jcc"

    # first, find a C compiler we can use
    CC=""
    has_tool cc && CC=cc
    has_tool jcc && CC=jcc
    has_tool gcc && CC=gcc
    has_tool clang && CC=clang

    if [ -z "$CC" ]; then
        printf "%b\n" "${BOLDRED}Could not find a C compiler! (tried 'cc', 'jcc', 'gcc', 'clang')${RESET}"

        cd - > /dev/null 2>&1
        exit 1
    fi

    flags=""
    flags="$flags -O3" # full opts
    flags="$flags -DJCC_ALL" # define JCC_ALL for a fat build

    if ! "$CC" $flags -DJCC_ALL -o "$output" $(find src -type f -name '*.c') -lm; then
        printf "%b\n" "${BOLDRED}Build failed!${RESET}"

        cd - > /dev/null 2>&1
        exit 1
    fi
fi


full="$(pwd)/${output}"
printf "%b\n" "${BOLD}JCC built to '$full'!${RESET}"

os="$(uname)"

if [ "$os" = "Darwin" ]; then
    target="$HOME/usr/local/bin"

    if ! try_root cp "$output" "$target"; then
        printf "%b\n" "${BOLDYELLOW}JCC built, but installing to '$target' failed${RESET}"

        cd - > /dev/null 2>&1
        exit 1
    fi

    printf "%b\n" "${BOLD}JCC installed to '$target'${RESET}"
elif [ "$os" = "Linux" ]; then
    target="/usr/local/bin"

    if ! try_root cp "$output" "$target"; then
        printf "%b\n" "${BOLDYELLOW}JCC built, but installing to '$target' failed${RESET}"

        cd - > /dev/null 2>&1
        exit 1
    fi

    printf "%b\n" "${BOLD}JCC installed to '$target'${RESET}"
else
    printf "%b\n" "${BOLDYELLOW}JCC built, but unsure how to install for OS '$os'${RESET}"

    cd - > /dev/null 2>&1
    exit 1
fi

# TODO: show if on path

cd - > /dev/null 2>&1
