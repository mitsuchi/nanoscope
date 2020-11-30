#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  echo "$input" | stack runghc nano.hs | lli
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 42 42

echo OK
