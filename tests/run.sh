#!/usr/bin/env bash

BOLD="\033[1m"
BOLDGREEN="\033[1;32m"
BOLDRED="\033[1;31m"
BOLDYELLOW="\033[1;33m"
RESET="\033[0m"

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

arch=""
for ((i=1; i<$#; i++)); do
  if [[ ${!i} == "-arch" ]]; then
    next_index=$((i+1))
    arch=${!next_index}
    break
  elif [[ ${!i} == "-T" ]]; then
    next_index=$((i+1))
    arch=${!next_index}
    arch=${arch%%-*}
    break
  fi
done

arch=${arch:-$(arch)}
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
  total=$((total+1))
done
pad=${#total}

completed=0
passed=0
failed=0
skipped=0

fails=()
skips=()

aggregator() {
  echo ""

  while read -r msg; do
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
    printf "${BOLD}\rCompleted %${pad}d/%d    ${BOLDGREEN}Pass: %${pad}d  ${BOLDRED}Fail: %${pad}d  ${BOLDYELLOW}Skip: %${pad}d${RESET}" \
      "$completed" "$total" "$passed" "$failed" "$skipped"
  done < "$fifo"

  echo ""

  if [ ${#fails[@]} -ne 0 ]; then
    echo -e ""

    echo -e "${BOLDRED}\nFailed tests:${RESET}"
    for reason in "${fails[@]}"; do
      echo -e "${BOLDRED}- $reason${RESET}"
    done

    echo -e ""
  fi

  if [ ${#skips[@]} -ne 0 ]; then
    echo -e ""

    echo -e "${BOLDYELLOW}Skipped tests:${RESET}"
    for reason in "${skips[@]}"; do
      echo -e "${BOLDYELLOW}- $reason${RESET}"
    done

    echo -e ""
  fi

  [ "$failed" -eq "0" ]
  exit $?
}


run_tests() {
  proc_id=$1
  shift

  for ((i=proc_id; i<${#all_files[@]}; i+=num_procs)); do
    file="${all_files[i]}"

    if [[ $file == *"/programs/"* ]]; then
      continue
    fi

    output="$proc_id.out"

    first_line=$(head -n 1 "$file")
    if [[ "$first_line" == "// no-compile" ]]; then
      if ./build/jcc "$@" -o "$output" -std=c23 -tm "$tm" "$file" >/dev/null 2>&1; then
        echo "fail File '$file' compiled successfully despite // no-compile" > "$fifo"
      else
        echo "pass" > "$fifo"
      fi
      continue
    fi

    target_arch=$(grep -i "arch" "$file" | head -1 | sed -n 's/^\/\/ arch: //p')
    if [[ -n $target_arch && $target_arch != "$arch" ]]; then
      echo "skip File '$file' skipped due to architecture (test: $target_arch, runner: $arch)" > "$fifo"
      continue
    fi

    expected=$(grep -i "expected value:" "$file" | head -1 | grep -Eo '[0-9]+')
    stdin=$(grep -i "stdin" "$file" | head -1 | sed -n 's/^\/\/ stdin: //p')
    stdout=$(grep -i "stdout" "$file" | head -1 | sed -n 's/^\/\/ stdout: //p')
    [ -z "$expected" ] && expected="0"

    if ! ./build/jcc "$@" -o "$output" -std=c23 -tm "$tm" "$file" >/dev/null 2>&1; then
      echo "fail File '$file' failed to compile" > "$fifo"
      continue
    fi

    if [ -z "$RUNNER" ]; then
      output_result=$(echo "$stdin" | ./"$output")
      result=$?
    else
      output_result=$(echo "$stdin" | "$RUNNER" "$output" 2>/dev/null)
      result=$?
    fi
  
    if [ "$result" != "$expected" ]; then
      echo "fail File '$file' produced exit code $result, expected $expected" > "$fifo"
    elif [ "$output_result" != "$stdout" ]; then
      echo "fail File '$file' output mismatch. Got: '$output_result', expected: '$stdout'" > "$fifo"
    else
      echo "pass" > "$fifo"
    fi
  done

  rm "$output"
}

num_procs=$(nproc 2> /dev/null || sysctl -n hw.physicalcpu 2>/dev/null || echo 4) # just assume 4 if needed

printf "${BOLD}Using %d processes...${RESET}\n" $num_procs

pids=()
for ((p=0; p<num_procs; p++)); do
  run_tests "$p" "$@" &
  pids+=($!)
done

aggregator &
agg_pid=$!

exec 3>$fifo

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
