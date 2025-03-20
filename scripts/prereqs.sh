#!/usr/bin/env bash

has_tool() {
    command -v "$1" &>/dev/null
}

try_root() {
    command -v sudo &>/dev/null && sudo "$@" || "$@"
}

# currently unused (if cmake is not installed, it will revert to slow build)
ensure-tool() {
  cmd="$1"
  pkg="${2:-$1}"

  echo "Checking for cmd '$cmd' (pkg='$pkg')..."

  has_tool "$cmd" && return

  os="$(uname)"

  if [ "$os" = "Darwin" ]; then
    echo "macOS detected."
    if ! command -v brew &>/dev/null; then
      echo "Homebrew missing, installing..."
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    brew update && brew install "$pkg"

  elif [ "$os" = "Linux" ]; then
    if [ -f /etc/debian_version ]; then
      echo "Debian/Ubuntu detected."
      try_root apt-get update
      try_root apt-get install -y "$pkg"
    elif [ -f /etc/redhat-release ]; then
      echo "Red Hat/CentOS/Fedora detected."
      if command -v dnf &>/dev/null; then
        try_root dnf install -y "$pkg"
      else
        try_root yum install -y "$pkg"
      fi
    elif [ -f /etc/arch-release ]; then
      echo "Arch Linux detected."
      try_root pacman -Sy --noconfirm "$pkg"
    else
      echo "Linux distro. Use your package manager manually."
      exit 1
    fi

  elif [[ "$os" =~ MINGW.* || "$os" =~ MSYS.* ]]; then
    echo "Windows (Git Bash/MSYS) detected."
    if command -v choco &>/dev/null; then
      choco install "$pkg" -y
    else
      echo "Chocolatey missing. Install it bc it's required."
      exit 1
    fi

  else
    echo "OS not supported - please manually install CMake."
    exit 1
  fi

  echo "CMake installed successfully."
}
