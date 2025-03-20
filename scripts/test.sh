#!/usr/bin/env bash

source ./scripts/profile.sh

BOLD="\033[1m"
BOLDGREEN="\033[1;32m"
BOLDRED="\033[1;31m"
BOLDYELLOW="\033[1;33m"
RESET="\033[0m"

VERBOSE_LEVEL="1"
PROFILE=""
ASSEMBLER=""
LINKER=""
JCC="build/jcc"

TEST_TIMEOUT="30s"
BUILD_TIMEOUT="1m"

TEST_DIR="../tests"

arg_groups=()
args=()
num_procs=$(nproc 2> /dev/null || sysctl -n hw.physicalcpu 2> /dev/null || { echo -e "${BOLDYELLOW}Could not find core count; defaulting to 4${RESET}" >&2; echo 4; }; )

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
  echo "    -n, --number "
  echo "        Run at most n tests"
  echo ""
  echo "    -j, --jobs "
  echo "        Use j processes"
  echo ""
  echo "    -p, --profile "
  echo "        Profile tests"
  echo ""
  echo "    --assembler <assembler> "
  echo "        Assemble instead of compiling, and use <assembler> to create object files. Requires '--linker'"
  echo ""
  echo "    --linker <linker> "
  echo "        Build object files (or assembly with '--assembler') and use <linker> to create executables"
  echo ""
  echo "    --arg-group <group> "
  echo "        Run every test with <group>. For example, \"--arg-group '-O1 -arch x86_64' --arg-group '-O2'\""
  echo "        will run every test with '-O1 -arch x86_64' and then '-O2'"
  echo ""
  echo "ARGS:"
  echo "    The args to be passed to JCC"
  echo ""
}

exp_file=""
all_files=()
while [[ $# -gt 0 ]]; do
  path=$( [[ -e "$1" ]] && echo "$1" || echo "$CALLER_DIR$1" )
  if [[ -f "$path" || -d "$path" ]]; then
    exp_file="1"

    echo -e "${BOLD}Adding '$path' to test list"

    if [[ -f "$path" ]]; then
      all_files+=("$path")
    elif [[ -d "$path" ]]; then
      while IFS= read -r file; do
        all_files+=("$file")
      done < <(find "$path" -type f -name "*.c")
    fi

    shift
    continue
  fi

  case "$1" in
    --help|-h|help)
      help
      exit 0
      ;;
    -j|--jobs)
      shift
      num_procs="$1"
      ;;
    -n|--number)
      shift
      num="$1"
      ;;
    --jcc)
      shift
      JCC="$CALLER_DIR$1"
      ;;
    --assembler)
      shift
      ASSEMBLER="$1"
      ;;
    --linker)
      shift
      LINKER="$1"
      ;;
    -p|--profile)
      PROFILE="1"
      ;;
    -q|--quiet)
      VERBOSE_LEVEL="0"
      ;;
    -v|--verbose)
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
    -arch)
      args+=("$1")
      shift
      args+=("$1")
      arch=$1
      ;;
    -target)
      args+=("$1")
      shift
      args+=("$1")
      arch=${1%%-*}
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

ensure_exists() {
  if ! command -v $1 &>/dev/null; then
    echo -e "${BOLDRED}$2${RESET}"
    exit -1
  fi
}

if [[ -n "$ASSEMBLER" ]]; then
  ensure_exists "$ASSEMBLER" "Assembler '$ASSEMBLER' could not be found"
fi

if [[ -n "$LINKER" ]]; then
  ensure_exists "$LINKER" "Linker '$LINKER' could not be found"
fi

if [[ -n "$ASSEMBLER" && -z "$LINKER" ]]; then
  echo -e "${BOLDRED}'--linker' must be set to use '--assembler'${RESET}"
  exit -1
fi

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


start_time=$(profile_begin)

if [ -n "$RUNNER" ] && ! command -v "$RUNNER" &>/dev/null; then
  echo -e "${BOLDRED}Runner '$RUNNER' could not be found! Exiting${RESET}"
  exit -1
fi

if [ ! -x "$JCC" ]; then
  echo "Expected file '$JCC' to be present & executable" >&2
  exit -1
fi

arch=${arch:-$(arch)}
arch=${arch/arm64/aarch64}

if [[ "$arch" == "rv32i" && -z "$RUNNER" ]]; then
  ensure_exists riscy "RUNNER not provided for arch $arch, but could not find default runner 'riscy'"

  echo -e "${BOLD}Defaulting to 'riscy' runner for $arch...${RESET}"
  RUNNER=riscy
fi

tm="Tue Dec 10 10:04:33 2024"

fifo=$(mktemp -u)
mkfifo "$fifo"

if [ -z $exp_file ]; then
  while IFS= read -r file; do
      all_files+=("$file")
  done < <(find "$(dirname "$0")/$TEST_DIR" -name '*.c' -print | sort)
fi

if ! [ -z $num ]; then
  # sort alphabetically for reproducability
  all_files=($(printf "%s\n" "${all_files[@]}" | sort))
  all_files=("${all_files[@]:0:num}")
fi

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
  pids="$@"

  echo ""

  while IFS= read -d '' -r msg; do
    status=${msg%% *}

    case "$status" in
      pass)
        passed=$((passed + 1))
        ;;
      fail)
        failed=$((failed + 1))
        fails+=("${msg#fail }")
        ;;
      skip)
        skipped=$((skipped + 1))
        skips+=("${msg#skip }")
        ;;
      *)
        continue
        ;;
    esac

    completed=$((completed + 1))

    if [ $VERBOSE_LEVEL -eq "1" ]; then
      printf "\r"
    fi
  
    if [ $VERBOSE_LEVEL -ge "1" ]; then
      printf "${BOLD}Completed %${pad}d/%d ($tests tests, $num_groups arg groups)    ${BOLDGREEN}Pass: %${pad}d  ${BOLDRED}Fail: %${pad}d  ${BOLDYELLOW}Skip: %${pad}d${RESET}" \
        "$completed" "$total" "$passed" "$failed" "$skipped"
    fi

    if [ $VERBOSE_LEVEL -gt "1" ]; then
      printf "\n"
    fi
  
  done < "$fifo"

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

  echo -e "${BOLD}====================${RESET}"
  echo -e "${BOLD}Total tests: $completed ${RESET}"
  echo -e "${BOLDGREEN}Passed: $passed ${RESET}"
  echo -e "${BOLDRED}Failed: $failed ${RESET}"
  echo -e "${BOLDYELLOW}Skipped: $skipped ${RESET}"
  
  echo ""

  [ "$failed" -eq "0" ]
  exit $?
}

TMP_DIR="tmp"
tmpname() {
  file=./$TMP_DIR/"$1"
  mkdir -p "$(dirname "$file")"
  echo "$file"
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
        status="$1"
        shift
        echo -ne "$status" "$@" '\0' > "$fifo"
      }

      if [ -n "$ASSEMBLER" ]; then
        build_command() {
          obj_files=()

          for file in "${files[@]}"; do
            asm=$(tmpname "$pid.$(basename "$file").s")
            timeout -k $BUILD_TIMEOUT $BUILD_TIMEOUT "$JCC" "${args[@]}" "${group_args[@]}" -S -o "$asm" -std=c23 -tm "$tm" "$file" \
              || return $?

            obj=$(tmpname "$pid.$(basename "$file").o")
            if [ $VERBOSE_LEVEL -ge "2" ]; then
              echo -e "$Running assembler '$ASSEMBLER "$asm" -o "$obj"'..."
            fi

            $ASSEMBLER "$asm" -o "$obj" || return $?
            obj_files+=("$obj")
          done

          if [ $VERBOSE_LEVEL -ge "2" ]; then
            echo -e "Running linker '$LINKER "${obj_files[@]}" -o "$output"'..."
          fi

          $LINKER "${obj_files[@]}" -o "$output" || return $?
        }
      elif [ -n "$LINKER" ]; then
        build_command() {
          obj_files=()

          for file in "${files[@]}"; do
            obj=$(tmpname "$pid.$(basename "$file").o")
            timeout -k $BUILD_TIMEOUT $BUILD_TIMEOUT "$JCC" "${args[@]}" "${group_args[@]}" -c -o "$obj" -std=c23 -tm "$tm" "$file" \
              || return $?

            obj_files+=("$obj")
          done

          if [ $VERBOSE_LEVEL -ge "2" ]; then
            echo -e "Running linker '$LINKER "${obj_files[@]}" -o "$output"'..."
          fi

          $LINKER "${obj_files[@]}" -o "$output" || return $?
        }
      else
        build_command() {
          timeout -k $BUILD_TIMEOUT $BUILD_TIMEOUT "$JCC" "${args[@]}" "${group_args[@]}" -o "$output" -std=c23 -tm "$tm" "${files[@]}"
          return $?
        }
      fi

      build() {
        if [[ $(uname) == "Darwin" ]]; then
          MallocNanoZone="0"
        fi

        MallocNanoZone=$MallocNanoZone build_command
        return $?
      }

      if [ "${#group_args[@]}" -ne 0 ]; then
        prefix="(Arg group: '${group_args[@]}'): "
      else
        prefix=""
      fi

      rm -f -- "$output"

      first_line=$(head -n 1 "$file")
      if [[ "$first_line" == "// no-compile" ]]; then
        if $(build &>/dev/null); then
          send_status fail "$prefix'$file' compiled successfully despite // no-compile"
        else
          send_status pass
        fi
        continue
      fi

      skip=$(grep -i "skip" "$file" | head -1 | sed -n 's/^\/\/ skip: //p')

      if [[ -n "$skip" ]]; then
        send_status skip "$prefix'$file' skipped: '$skip'"
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

      if [ "$VERBOSE_LEVEL" -ge "2" ]; then
        echo -e "Compiling files '${files[@]}' with 'jcc " "${args[@]}" "${group_args[@]}" -o "$output" -std=c23 -tm "$tm" "${files[@]}" "'${RESET}\n"
      fi

      if ! build_msg=$(build 2>&1); then
        send_status fail "$prefix'$file' failed to compile. Build output: \n${RESET}$(echo "$build_msg" | awk '{print "  " $0}')${RESET}\n"
        continue
      fi

      if ! [ -f "$output" ]; then
        send_status fail "$prefix'$file' compiled, but no output file was found. Build output: \n${RESET}$(echo "$build_msg" | awk '{print "  " $0}')${RESET}\n"
        continue
      fi

      if [ $VERBOSE_LEVEL -ge "2" ]; then
        echo -e "${BOLD}Running output...${RESET}"
      fi

      # supress echo stderr because otherwise we get spurious broken pipe errors
      if [ -z "$RUNNER" ]; then
        output_result=$(echo "$stdin" 2>/dev/null | timeout -k $TEST_TIMEOUT $TEST_TIMEOUT ./"$output" 2>/dev/null)
        result=$?
      else
        output_result=$(echo "$stdin" 2>/dev/null | timeout -k $TEST_TIMEOUT $TEST_TIMEOUT "$RUNNER" "$output" 2>/dev/null)
        result=$?
      fi
  
      if [ "$result" != "$expected" ]; then
        send_status fail "$prefix'$file' produced exit code $result, expected $expected. Build output: \n${RESET}$(echo "$build_msg" | awk '{print "  " $0}')${RESET}\n"
      elif [ "$output_result" != "$stdout" ]; then
        output_result=${output_result//$'\n'/\\n}
        stdout=${stdout//$'\n'/\\n}
        send_status fail "$prefix'$file' output mismatch. Got: '$output_result', expected: '$stdout'. Build output: \n${RESET}$(echo "$build_msg" | awk '{print "  " $0}')${RESET}\n"
      else
        send_status pass
      fi
    done
  done
}

if [ $VERBOSE_LEVEL -ge "2" ]; then
  num_procs=1
  printf "${BOLD}Using 1 process due to '-v|--verbose'...${RESET}\n"
else
  printf "${BOLD}Using %d processes...${RESET}\n" $num_procs
fi

pids=()
tmps=()
for ((p=0; p<num_procs; p++)); do
  output="$p.out"
  tmps+=($output)

  run_tests "$p" "$output" "$@" &
  pids+=($!)
done

clean() {
  for pid in "${pids[@]}"; do
    kill "$pid" &>/dev/null
    wait "$pid" &>/dev/null
  done

  # for some reason this causes cleanup to work
  sleep 0.1

  rm -r "$TMP_DIR" &>/dev/null
  rm -f "${tmps[@]}"
}

trap 'clean' EXIT SIGINT INT

aggregator "${pids[@]}" &
agg_pid=$!

exec 3>$fifo &>/dev/null

for pid in "${pids[@]}"; do
  wait "$pid"
done

exec 3>&-

wait "$agg_pid"
exc=$?

profile_end "Tests took " $start_time

exit $exc
