#!/usr/bin/env bash

BOLD="\033[1m"
BOLDGREEN="\033[1;32m"
BOLDRED="\033[1;31m"
BOLDYELLOW="\033[1;33m"
RESET="\033[0m"

VERBOSE_LEVEL="1"
arg_groups=()
args=()
num_procs=$(nproc 2> /dev/null || sysctl -n hw.physicalcpu 2> /dev/null || echo 4) # just assume 4 if needed

help() {
  echo "JCC test runner"
  echo "John Kelly <johnharrykelly@gmail.com>"
  echo ""
  echo "jcc.sh test [-h|--help] [--quiet] [--verbose] [--arg-group <group>] [ARGS]"
  echo ""
  echo "OPTIONS:"
  echo "    --quiet "
  echo "        Run in quiet mode, not printing live progress "
  echo ""
  echo "    --verbose "
  echo "        Run in verbose mode"
  echo ""
  echo "    --arg-group <group> "
  echo "        Run every test with <group>. For example, \"--arg-group '-O1 -arch x86_64' --arg-group '-O2'\""
  echo "        will run every test with '-O1 -arch x86_64' and then '-O2'"
  echo ""
  echo "ARGS:"
  echo "    The args to be passed to JCC"
  echo ""
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h|help)
      help
      exit 0
      ;;
    -j|--jobs)
      shift
      num_procs="$1"
      ;;
    --quiet)
      VERBOSE_LEVEL="0"
      ;;
    --verbose)
      VERBOSE_LEVEL="2"
      ;;
    --arg-group)
      shift
      if [[ $# -gt 0 ]]; then
        arg_groups+=("$1")
      else
        echo "Error: --arg-group requires an argument" >&2
        exit 1
      fi
      ;;
    --)
      shift
      args+=("$@")
      break
      ;;
    *)
      args+=("$1")
      ;;
  esac
  shift
done

if [ "${#args[@]}" -ne 0 ]; then
  delim=$" "
  rest=("${args[@]:1}")
  printf "${BOLD}Running tests with '"
  printf "%s" "${args[0]}" "${rest[@]/#/$delim}"
  printf "'${RESET}\n"
fi

no_groups=""
if [[ ${#arg_groups[@]} -eq 0 ]]; then
  no_groups="1"
  arg_groups=("")
else
  delim=$"', '"
  rest=("${arg_groups[@]:1}")
  printf "${BOLD}Running arg groups '"
  printf "%s" "${arg_groups[0]}" "${rest[@]/#/$delim}"
  printf "'${RESET}\n"
fi

if command -v gdate &>/dev/null; then
  date="gdate"
elif date --version 2>/dev/null | grep -q "GNU coreutils"; then
  date="date"
else
  echo -e "${BOLDYELLOW}GNU date not available, not timestamping. Install coreutils or use gdate.${RESET}"
fi

if [ -n "$date" ]; then
  start_time=$($date +%s%3N)
fi

if [ -n "$RUNNER" ] && ! command -v "$RUNNER" &>/dev/null; then
  echo -e "${BOLDRED}Runner '$RUNNER' could not be found! Exiting${RESET}"
  exit -1
fi

if [ ! -x build/jcc ]; then
  echo "Expected file 'build/jcc' to be present & executable" >&2
  exit -1
fi

arch="$JCC_ARCH"
if [ -z "$arch"]; then
  for ((i=1; i<$#; i++)); do
    if [[ ${!i} == "-arch" ]]; then
      next_index=$((i+1))
      arch=${!next_index}
      break
    elif [[ ${!i} == "-target" ]]; then
      next_index=$((i+1))
      arch=${!next_index}
      arch=${arch%%-*}
      break
    fi
  done

  arch=${arch:-$(arch)}
fi

arch=${arch/arm64/aarch64}

tm="Tue Dec 10 10:04:33 2024"

fifo=$(mktemp -u)
mkfifo "$fifo"

all_files=()
while IFS= read -r file; do
    all_files+=("$file")
done < <(find "$(dirname "$0")" -name '*.c' -print | sort)

total=0
for file in "${all_files[@]}"; do
  [[ $file == *"/programs/"* ]] && continue
  [[ "$file" == *_driver.c ]] && continue

  total=$((total+1))
done
pad=${#total}

tests=$total
num_groups=${#arg_groups[@]}
total=$((total * num_groups))

completed=0
passed=0
failed=0
skipped=0

fails=()
skips=()

if [ -z "$no_groups" ]; then
  printf "${BOLD}Running $total tests with $num_groups arg groups${RESET}\n"
else
  printf "${BOLD}Running $total tests\n"
fi


aggregator() {
  echo ""

  while IFS= read -d '' -r msg; do
    completed=$((completed + 1))
    case "$msg" in
      pass)
        passed=$((passed + 1))
        ;;
      fail\ *)
        failed=$((failed + 1))
        fails+=("${msg#fail }")
        ;;
      skip\ *)
        skipped=$((skipped + 1))
        skips+=("${msg#skip }")
        ;;
    esac

    if [ $VERBOSE_LEVEL -ge "1" ]; then
      printf "${BOLD}\rCompleted %${pad}d/%d ($tests tests, $num_groups arg groups)    ${BOLDGREEN}Pass: %${pad}d  ${BOLDRED}Fail: %${pad}d  ${BOLDYELLOW}Skip: %${pad}d${RESET}" \
        "$completed" "$total" "$passed" "$failed" "$skipped"
    fi
  done < "$fifo"

  echo ""

  echo -e "${BOLD}====================${RESET}"
  echo -e "${BOLD}Total tests: $completed ${RESET}"
  echo -e "${BOLDGREEN}Passed: $passed ${RESET}"
  echo -e "${BOLDRED}Failed: $failed ${RESET}"
  echo -e "${BOLDYELLOW}Skipped: $skipped ${RESET}"
  
  echo ""

  if [ ${#fails[@]} -ne 0 ]; then
    echo -e ""

    echo -e "${BOLDRED}\nFailed tests:${RESET}"
    for reason in "${fails[@]}"; do
      echo -ne "${BOLDRED}- "
      echo -ne "$reason"
      echo -e "${RESET}"
    done

    echo -e ""
  fi

  if [ ${#skips[@]} -ne 0 ]; then
    echo -e ""

    echo -e "${BOLDYELLOW}Skipped tests:${RESET}"
    for reason in "${skips[@]}"; do
      echo -ne "${BOLDYELLOW}- "
      echo -n "$reason"
      echo -e "${RESET}"
    done

    echo -e ""
  fi

  [ "$failed" -eq "0" ]
  exit $?
}

run_tests() {
  proc_id=$1
  output=$2
  shift 2

  for ((i=proc_id; i<${#all_files[@]}; i+=num_procs)); do
    file="${all_files[i]}"

    # program tests aren't run because output can't be checked for correctness
    if [[ "$file" == *"/programs/"* ]]; then
      continue
    fi

    # langproc tests have a different structure, where there are two files
    # (`foo.c` and `foo_driver.c`) and you compile them together
    local langproc
    if [[ "$file" == *"/langproc/"*  ]]; then
      if [[ "$file" == *_driver.c ]]; then
        continue
      fi

      langproc="1"
      files=("$file" "${file%.c}_driver.c")
    else
      files=("$file")
    fi

    for arg_group in "${arg_groups[@]}"; do
      read -a group_args <<< "$arg_group"

      send_status() {
        echo -ne "$@" '\0' > "$fifo"
      }

      build() {
        if [ $VERBOSE_LEVEL -ge "2" ]; then
          echo -e "\n${BOLD} Running 'jcc " "${args[@]}" "${group_args[@]}" -o "$output" -std=c23 -tm "$tm" "${files[@]}" "'${RESET}\n"
        fi

        ./build/jcc "${args[@]}" "${group_args[@]}" -o "$output" -std=c23 -tm "$tm" "${files[@]}"
        return $?
      }

      if [ "${#group_args[@]}" -ne 0 ]; then
        prefix="(Arg group: '${group_args[@]}'): "
      else
        prefix=""
      fi

      first_line=$(head -n 1 "$file")
      if [[ "$first_line" == "// no-compile" ]]; then
        if $(build &>/dev/null); then
          send_status fail "$prefix'$file' compiled successfully despite // no-compile"
        else
          send_status pass
        fi
        continue
      fi

      target_arch=$(grep -i "arch" "$file" | head -1 | sed -n 's/^\/\/ arch: //p')
      target_arch=${target_arch/arm64/atarget_arch64}

      if [[ -n $target_arch && $target_arch != "$arch" ]]; then
        send_status skip "$prefix'$file' skipped due to architecture (test: $target_arch, runner: $arch)"
        continue
      fi

      expected=$(grep -i "expected value:" "$file" | head -1 | grep -Eo '[0-9]+')
      stdin=$(grep -i "stdin" "$file" | head -1 | sed -n 's/^\/\/ stdin: //p')
      stdout=$(grep -i "stdout" "$file" | head -1 | sed -n 's/^\/\/ stdout: //p')
      [ -z "$expected" ] && expected="0"

      if ! build_msg=$(build 2>&1); then
        send_status fail "$prefix'$file' failed to compile. Build output: \n${RESET}$(echo "$build_msg" | awk '{print "  " $0}')${RESET}\n"
        continue
      fi

      # supress echo stderr because otherwise we get spurious broken pipe errors
      if [ -z "$RUNNER" ]; then
        output_result=$(echo "$stdin" 2>/dev/null | ./"$output" 2>/dev/null)
        result=$?
      else
        output_result=$(echo "$stdin" 2>/dev/null | "$RUNNER" "$output" 2>/dev/null)
        result=$?
      fi
  
      if [ "$result" != "$expected" ]; then
        echo "fail File '$file' produced exit code $result, expected $expected" > "$fifo"
      elif [ "$output_result" != "$stdout" ]; then
        output_result=${output_result//$'\n'/\\n}
        stdout=${stdout//$'\n'/\\n}
        send_status fail "$prefix'$file' output mismatch. Got: '$output_result', expected: '$stdout'" '\0' > "$fifo"
      else
        send_status pass
      fi
    done
  done
}

printf "${BOLD}Using %d processes...${RESET}\n" $num_procs

pids=()
tmps=()
for ((p=0; p<num_procs; p++)); do
  output="$p.out"
  tmps+=($output)

  run_tests "$p" "$output" "$@" &
  pids+=($!)
done

aggregator &
agg_pid=$!

exec 3>$fifo

clean() {
  for pid in "${pids[@]}"; do
    kill "$pid" &>/dev/null
    wait "$pid" &>/dev/null
  done

  # for some reason this causes cleanup to work
  sleep 0.1

  rm -f "${tmps[@]}"
}

trap clean EXIT
trap 'clean; exit -1' SIGINT

for pid in "${pids[@]}"; do
  wait "$pid"
done

exec 3>&-

wait "$agg_pid"
exc=$?

if [ -n "$date" ]; then
  end_time=$($date +%s%3N)
  elapsed=$((end_time - start_time))

  printf "${BOLD}Tests took %d.%03d${RESET}s\n" $((elapsed / 1000)) $((elapsed % 1000))
fi

exit $exc
