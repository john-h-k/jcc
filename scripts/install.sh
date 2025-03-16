#!/bin/sh

# This is a standalone - not used by other scripts
# needs lots of logic improvements this is a vEpsilon

set -e

BOLD="\033[1m"
BOLDRED="\033[1;31m"
BOLDYELLOW="\033[1;33m"
RESET="\033[0m"

has_tool() {
 command -v "$1" > /dev/null 2>&1
 return $?
}

mkdir -p jcc
if has_tool git; then
    printf "%b\n" "${BOLD}Downloading with 'git clone'...${RESET}"
    git clone https://github.com/john-h-k/jcc jcc
else
    if has_tool curl; then
        printf "%b\n" "${BOLD}Downloading tarball with 'curl'...${RESET}"
        curl -L https://github.com/john-h-k/jcc/archive/refs/heads/main.tar.gz | tar xz --strip-components=1 -C jcc
    elif has_tool wget; then
        printf "%b\n" "${BOLD}Downloading tarball with 'wget'...${RESET}"
        wget -q0- https://github.com/john-h-k/jcc/archive/refs/heads/main.tar.gz | tar xz --strip-components=1 -C jcc
    else
        # could be that the script has been copied over file system or similar
        printf "%b\n" "${BOLDRED}'curl' or 'wget' required to install JCC${RESET}"
        exit 1
    fi
fi

printf "%b\n\n" "${BOLD}Downloading done!${RESET}"

# first, find a C compiler we can use
CC=""
has_tool cc && CC=cc
has_tool jcc && CC=jcc
has_tool gcc && CC=gcc
has_tool clang && CC=clang

if [ -z "$CC" ]; then
    printf "%b\n" "${BOLDRED}Could not find a C compiler! (tried 'cc', 'jcc', 'gcc', 'clang')${RESET}"
    exit 1
fi

cd jcc

output=""
if has_tool bash; then
    output="build/jcc"
    ./jcc.sh build
else
    printf "%b\n" "${BOLD} Bash is not installed; reverting to a simple build...$RESET"
    # if bash isn't installed, manually do a simple build

    mkdir -p build
    output="build/jcc"
    if ! "$CC" -DJCC_ALL -o "$output" $(find src -type f -name '*.c') -lm; then
          printf "%\b\n" "${BOLDRED}Build failed!${RESET}"
          exit 1
    fi
fi

full="$(pwd)/${output}"
printf "%b\n" "${BOLD}JCC installed to '$full'!${RESET}"

cd - > /dev/null 2>&1
