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

assert 0   "define main(): i32 { return 0; }"
assert 42  "define main(): i32 { return 42; }"
assert 5   "define main(): i32 { return 2+3; }"
assert 4   "define main(): i32 { return 11-7; }"
assert 26  "define main(): i32 { return 5+112-91; }"
assert 10  "define main(): i32 { return 5*2; }"
assert 4   "define main(): i32 { return 20/5; }"
assert 11  "define main(): i32 { return 3+2*4; }"
assert 25  "define main(): i32 { return 5*(2+3); }"
assert 35  "define main(): i32 { return 7*(3+4/2); }"
assert 7   " define main ( ) : i32  { return 2 * (3 + 4)  / 2 ; } "
assert 15  "define main(): i32 { return 2+++13; }"
assert 17  "define main(): i32 { return 23---6; }"
assert 13  "define main(): i32 { return -10+23; }"
assert 12  "define main(): i32 { return 2+2*5; return 25; }"
assert 31  "define main(): i32 { 52; return 31; 47; }"
assert 52  "define main(): i32 { dec chainsaw: i32 = 13; chainsaw = chainsaw * 4; return chainsaw; }"
assert 1   "define main(): i32 { dec __: i32, _return: i32, __a__: i8; return 1; }"
assert 5   "define main(): i32 { dec a: i32 = 7, b: i32; a = b = 5; return a; }"
assert 1   "define main(): i32 { return 1==1; }"
assert 0   "define main(): i32 { return 1==5; }"
assert 0   "define main(): i32 { return 1!=1; }"
assert 1   "define main(): i32 { return 1!=5; }"
assert 1   "define main(): i32 { return 0<1; }"
assert 0   "define main(): i32 { return 1<1; }"
assert 0   "define main(): i32 { return 2<1; }"
assert 1   "define main(): i32 { return 0<=1; }"
assert 1   "define main(): i32 { return 1<=1; }"
assert 0   "define main(): i32 { return 2<=1; }"
assert 0   "define main(): i32 { return 0>1; }"
assert 0   "define main(): i32 { return 1>1; }"
assert 1   "define main(): i32 { return 2>1; }"
assert 0   "define main(): i32 { return 0>=1; }"
assert 1   "define main(): i32 { return 1>=1; }"
assert 1   "define main(): i32 { return 2>=1; }"
assert 32  "define main(): i32 { 10; { 21; return 32; } return 57; }"
assert 4   "define main(): i32 { dec a: i32 = 1; if a<1: return 3; return 4; }"
assert 3   "define main(): i32 { if 1==1: return 3; else return 4; }"
assert 3   "define ret3(): i32 { return 3; } define main(): i32 { return ret3(); }"
assert 15  "define add5(a: i32, b: i32, c: i32, d: i32, e: i32): i32 { return a+b+c+d+e; } define main(): i32 { return add5(1,2,3,4,5); }"
assert 89  "define fibo(a: i32): i32 { if a<=1: return 1; return fibo(a-1) + fibo(a-2); } define main(): i32 { return fibo(10); }"
assert 56  "define main(): i32 { dec a: i32 = 22, b: i8 = 34; a = a + b; b = a; return b; }"
assert 56  "define add(a: i8, b: i32): i8 { return a + b; } define main(): i32 { dec a: i32 = 22, b: i8 = 34; return add(a, b); }"
assert 10  "struct s1 { a: i32, b: i8 } define main(): i32 { dec a: s1; a.b = 10; return a.b; }"
assert 5   "struct s1 { a: i32, b: i8 } struct s2{ c: s1, d: i32 } struct s3{ e: s2, f: s1 } define main(): i32 { dec a: s3; a.e.c.b = 5; return a.e.c.b; }"
assert 3   "define main(): i32 { dec a: i32 = 1; { dec b: i32 = 2; { dec c: i32 = 3; b = c; } { dec d: i32 = b; } a = b; } return a; }"
assert 1   "skill skill_test { define skill_func(): i32 { dec a: i32 = 10; dec b: i32 = 5; return a + b; } } define main(): i32 { return 1; }"
assert 10  "skill ret_skill { define ret_10(): i32 { return 10; } } define main(): i32 { |ret_skill| { return ret_10(); } }"
assert 3   "skill extend_test { extend i32 { a: i32, b: i8 } } define main(): i32 { |extend_test| { dec x: i32 = 3; return x; } return 5; }"
assert 3   "struct hoge { h1: i32, h2: i8, h3: i32 } skill extend_test { extend i32 { a: i32, b: hoge } extend hoge { c: i32, b: hoge } } define main(): i32 { |extend_test| { dec x: i32 = 3, y: hoge; return x; } return 5; }"
assert 5   "skill extend_add3 {
                extend i32 { 
                    plus3: i32 
                } 
                define add3(origin x: i32): i32 { 
                    x = x + 3; 
                    return x; 
                } 
                define get3(origin x: i32): i32 { 
                    return x; 
                } 
            } 
            define main(): i32 { 
                |extend_add3| { 
                    dec x: i32 = 5; 
                    add3(x); 
                    return get3(x); 
                } 
            }"

echo OK 