#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  echo "$input" | stack run | lli
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 42 42
assert 46 12+34
assert 4 5-1
assert 7 10-1-2
assert 2 10-9+8-7
assert 6 2*3
assert 2 10/4
assert 7 1+2*3
assert 9 "(1+2)*3"
assert 3 "a=1; b=2; a+b"
assert 3 "if 1 < 2 then 3 else 4"
assert 4 "a=5; if a < 2 then 3 else 4"

echo OK
