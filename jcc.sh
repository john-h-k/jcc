#!/bin/bash

format() {
  fd '.*\.[hc]' . -x clang-format -style=file -i
}

invoke-subcommand() {
  local base name func

  base=$1

  unset name
  if [ -z "${1}" ]; then
      print "No subcommand provided; defaulting to 'help' subcommand..."
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
      local func_names=(${(Mok)functions:#$base?*}) # don't match private _ lead functions

      print "Usage: ${base} <subcommand> [options]"
      print "Subcommands:"
      for func in $func_names; do
          print "    - $base ${func#${base}-}"
      done
  else
      local matches=(${(Mok)functions:#$base-$name*})

      if [[ ${#matches[@]} == 1 ]]; then
          "${matches[1]}" "${@}"
      elif [[ ${#matches[@]} > 1 ]]; then
          print "'${name}' is ambiguous; did you mean one of the following?" >&2
          for match in $matches; do
              print "    - $base ${match#${base}-}" >&2
          done
          return 1
      else
          print "'${name}' - not valid subcommand for '${base}'" >&2
          return 1
      fi
  fi
}

invoke-subcommand "$@"

