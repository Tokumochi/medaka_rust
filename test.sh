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

assert 0 "define main(): i32 { return 0; }"
assert 42 "define main(): i32 { return 42; }"
assert 5 "define main(): i32 { return 2+3; }"
assert 4 "define main(): i32 { return 11-7; }"
assert 26 "define main(): i32 { return 5+112-91; }"
assert 10 "define main(): i32 { return 5*2; }"
assert 4 "define main(): i32 { return 20/5; }"
assert 11 "define main(): i32 { return 3+2*4; }"
assert 25 "define main(): i32 { return 5*(2+3); }"
assert 35 "define main(): i32 { return 7*(3+4/2); }"
assert 7 " define main ( ) : i32  { return 2 * (3 + 4)  / 2 ; } "
assert 15 "define main(): i32 { return 2+++13; }"
assert 17 "define main(): i32 { return 23---6; }"
assert 13 "define main(): i32 { return -10+23; }"
assert 12 "define main(): i32 { return 2+2*5; return 25; }"
assert 31 "define main(): i32 { 52; return 31; 47; }"
assert 52 "define main(): i32 { dec chainsaw: i32 = 13; chainsaw = chainsaw * 4; return chainsaw; }"
assert 5 "define main(): i32 { dec a: i32 = 7, b: i32; a = b = 5; return a; }"
assert 1 "define main(): i32 { return 1==1; }"
assert 0 "define main(): i32 { return 1==5; }"
assert 0 "define main(): i32 { return 1!=1; }"
assert 1 "define main(): i32 { return 1!=5; }"
assert 1 "define main(): i32 { return 0<1; }"
assert 0 "define main(): i32 { return 1<1; }"
assert 0 "define main(): i32 { return 2<1; }"
assert 1 "define main(): i32 { return 0<=1; }"
assert 1 "define main(): i32 { return 1<=1; }"
assert 0 "define main(): i32 { return 2<=1; }"
assert 0 "define main(): i32 { return 0>1; }"
assert 0 "define main(): i32 { return 1>1; }"
assert 1 "define main(): i32 { return 2>1; }"
assert 0 "define main(): i32 { return 0>=1; }"
assert 1 "define main(): i32 { return 1>=1; }"
assert 1 "define main(): i32 { return 2>=1; }"
assert 32 "define main(): i32 { 10; { 21; return 32; } return 57; }"
assert 4 "define main(): i32 { dec a: i32 = 1; if a<1: return 3; return 4; }"
assert 3 "define main(): i32 { if 1==1: return 3; else return 4; }"
assert 3 "define ret3(): i32 { return 3; } define main(): i32 { return ret3(); }"
assert 15 "define add5(a: i32, b: i32, c: i32, d: i32, e: i32): i32 { return a+b+c+d+e; } define main(): i32 { return add5(1,2,3,4,5); }"
assert 89 "define fibo(a: i32): i32 { if a<=1: return 1; return fibo(a-1) + fibo(a-2); } define main(): i32 { return fibo(10); }"
assert 56 "define main(): i32 { dec a: i32 = 22, b: i8 = 34; a = a + b; b = a; return b; }"
assert 56 "define add(a: i8, b: i32): i8 { return a + b; } define main(): i32 { dec a: i32 = 22, b: i8 = 34; return add(a, b); }"
assert 10 "struct struc { a: i32, b: i8 } define main(): i32 { return 10; }"

echo OK 