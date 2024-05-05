#!/bin/sh

ls jcc >/dev/null 2>&1
if [ $? != 0 ]; then
  echo "Expected file \`jcc\` to be present & executable"
  exit -1
fi

for file in $(find $(dirname $0) -name '*.c' -print); do
  echo "Testing $file..."
  expected=$(head -1 $file | grep -Eo '[0-9]+')
  if ! ./jcc $file >/dev/null 2>&1; then
    echo "compilation failed!"
    # exit -1
  fi

  ./a.out
  result=$?
  if [ $result -ne $expected ]; then
    echo "TEST FAILED: expected value $expected, got $result"
  else
    echo "TEST PASSED"
  fi

  echo "\n\n"
done

