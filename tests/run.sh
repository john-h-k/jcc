#!/bin/sh

ls jcc >/dev/null 2>&1
if [ $? != 0 ]; then
  echo "Expected file \`jcc\` to be present & executable"
  exit -1
fi

for file in $(find $(dirname $0) -name '*.c' -print | sort); do
  echo "Testing $file..."
  expected=$(grep -i "Expected value:" $file | head -1 | grep -Eo '[0-9]+')
  stdin=$(grep -i "stdin" $file | head -1 | sed -n 's/^\/\/ stdin: //p')
  stdout=$(grep -i "stdout" $file | head -1 | sed -n 's/^\/\/ stdout: //p')

  if [ -z "$expected" ]; then
    expected="0"
  fi

  if ! ./jcc $file >/dev/null 2>&1; then
    echo "compilation failed!"
    # exit -1
  fi

  output=$(echo "$stdin" | ./a.out)

  result=$?
  if [ "$result" != "$expected" ]; then
    echo "TEST FAILED: expected return code $expected, got $result"
    break
  elif [ "$output" != "$stdout" ]; then
    echo "TEST FAILED: expected stdout '$stdout', got '$output'"
    break
  else
    echo "TEST PASSED"
  fi

  echo "\n"
done

