#!/usr/bin/env bash

ls build/jcc >/dev/null 2>&1
if [ $? != 0 ]; then
  echo "Expected file \`build/jcc\` to be present & executable"
  exit -1
fi

tm="Tue Dec 10 10:04:33 2024"

for file in $(find $(dirname $0) -name '*.c' -print | sort); do
  if [[ $file == *"/programs/"* ]]; then
    continue
  fi

  echo "Testing $file..."

  first_line=$(head -n 1 "$file")
  if [[ "$first_line" == "// no-compile" ]]; then
    if ./build/jcc "$@" -std=c23 -tm "$tm" "$file" >/dev/null 2>&1; then
      echo "TEST FAILED: expected compilation to fail, but it succeeded"
      exit -1
    else
      echo "TEST PASSED: compilation failed as expected"
    fi
  else
    expected=$(grep -i "Expected value:" $file | head -1 | grep -Eo '[0-9]+')
    stdin=$(grep -i "stdin" $file | head -1 | sed -n 's/^\/\/ stdin: //p')
    stdout=$(grep -i "stdout" $file | head -1 | sed -n 's/^\/\/ stdout: //p')

    if [ -z "$expected" ]; then
      expected="0"
    fi

    if ! ./build/jcc "$@" -std=c23 -tm "$tm" $file >/dev/null 2>&1; then
      echo "compilation failed!"
      exit -1
    fi

    output=$(echo "$stdin" | ./a.out)

    result=$?
    if [ "$result" != "$expected" ]; then
      echo "TEST FAILED: expected return code $expected, got $result"
      exit -1
    elif [ "$output" != "$stdout" ]; then
      echo "TEST FAILED: expected stdout '$stdout', got '$output'"
      exit -1
    else
      echo "TEST PASSED"
    fi
  fi


  echo -e "\n"
done

