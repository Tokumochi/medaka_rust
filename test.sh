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

assert 0 "{ return 0; }"
assert 42 "{ return 42; }"
assert 5 "{ return 2+3; }"
assert 4 "{ return 11-7; }"
assert 26 "{ return 5+112-91; }"
assert 10 "{ return 5*2; }"
assert 4 "{ return 20/5; }"
assert 11 "{ return 3+2*4; }"
assert 25 "{ return 5*(2+3); }"
assert 35 "{ return 7*(3+4/2); }"
assert 7 " { return 2 * (3 + 4)  / 2 ; } "
assert 15 "{ return 2+++13; }"
assert 17 "{ return 23---6; }"
assert 13 "{ return -10+23; }"
assert 12 "{ return 2+2*5; return 25; }"
assert 31 "{ 52; return 31; 47; }"
assert 52 "{ dec chainsaw: i32 = 13; chainsaw = chainsaw * 4; return chainsaw; }"
assert 1 "{ return 1==1; }"
assert 0 "{ return 1==5; }"
assert 0 "{ return 1!=1; }"
assert 1 "{ return 1!=5; }"
assert 1 "{ return 0<1; }"
assert 0 "{ return 1<1; }"
assert 0 "{ return 2<1; }"
assert 1 "{ return 0<=1; }"
assert 1 "{ return 1<=1; }"
assert 0 "{ return 2<=1; }"
assert 0 "{ return 0>1; }"
assert 0 "{ return 1>1; }"
assert 1 "{ return 2>1; }"
assert 0 "{ return 0>=1; }"
assert 1 "{ return 1>=1; }"
assert 1 "{ return 2>=1; }"
assert 32 "{ 10; { 21; return 32; } return 57; }"

echo OK 