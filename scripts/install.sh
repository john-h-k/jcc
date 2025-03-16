#!/usr/bin/env bash

# This is a standalone - not used by other scripts
# needs lots of logic improvements this is a vEpsilon

set -e

BOLD="\033[1m"
BOLDRED="\033[1;31m"
RESET="\033[0m"

mkdir -p jcc
if command -v git &>/dev/null; then
  echo -e "${BOLD}Downloading with 'git clone'...${RESET}"
  git clone https://github.com/john-h-k/jcc jcc
else
  echo -e "${BOLD}Downloading tarball...${RESET}"
  curl -L https://github.com/john-h-k/jcc/archive/refs/heads/main.tar.gz | tar xz --strip-components=1 -C jcc
fi

echo -e "${BOLD}Downloading done!${RESET}"

cd jcc
./jcc.sh build
