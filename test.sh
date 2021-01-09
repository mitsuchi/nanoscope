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

assert 42 "return 42"
assert 46 "return 12+34"
assert 4 "return 5-1"
assert 7 "return 10-1-2"
assert 2 "return 10-9+8-7"
assert 6 "return 2*3"
assert 2 "return 10/4"
assert 7 "return 1+2*3"
assert 9 "return (1+2)*3"
assert 3 "a=1; b=2; return a+b;"

echo OK
