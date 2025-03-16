#!/usr/bin/env bash

ensure_cmake() {
  os="$(uname)"

  if [ "$os" = "Darwin" ]; then
    echo "macOS detected."
    if ! command -v brew &>/dev/null; then
      echo "Homebrew missing, installing..."
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    brew update && brew install cmake

  elif [ "$os" = "Linux" ]; then
    if [ -f /etc/debian_version ]; then
      echo "Debian/Ubuntu detected."
      sudo apt-get update && sudo apt-get install -y cmake
    elif [ -f /etc/redhat-release ]; then
      echo "Red Hat/CentOS/Fedora detected."
      if command -v dnf &>/dev/null; then
        sudo dnf install -y cmake
      else
        sudo yum install -y cmake
      fi
    elif [ -f /etc/arch-release ]; then
      echo "Arch Linux detected."
      sudo pacman -Sy --noconfirm cmake
    else
      echo "Linux distro. Use your package manager manually."
      exit 1
    fi

  elif [[ "$os" =~ MINGW.* || "$os" =~ MSYS.* ]]; then
    echo "Windows (Git Bash/MSYS) detected."
    if command -v choco &>/dev/null; then
      choco install cmake -y
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
