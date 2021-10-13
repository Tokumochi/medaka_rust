#!/bin/bash
assert() {
    expected="$1"
    input="$2"

    target/debug/medaka_rust "$input" > tmp.ll || exit
    lli tmp.ll
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
}

assert 0 0
assert 42 42
assert 5 2+3
assert 4 11-7
assert 26 5+112-91
assert 10 5*2
assert 4 20/5
assert 11 3+2*4

echo OK 