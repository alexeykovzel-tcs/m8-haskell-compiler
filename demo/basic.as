/*
    This file demonstrates the basic functionality of the compiler
    such as variables, functions, arrays, loops and conditions.
*/

/* ------------------------ */
/*         variables        */
/* ------------------------ */

let v1: Int = 4;
let v2: Char = 'a';
let v3: Bool = true;

print(v2);  // result: 97 (ordinal value for 'a')
print(v1);  // result: 4
print(v3);  // result: 1 (true) 

/* ------------------------ */
/*         functions        */
/* ------------------------ */

fun say_hello() {
    print_str("hello");
}

fun decr(n: Int) -> Int {
    return n - 1;
}

let ten: Int = decr(11);

print(ten);   // result: 10
say_hello();  // result: "hello"

/* ------------------------ */
/*          arrays          */
/* ------------------------ */

let arr1: Int[4] = [1, 2, 3, 4];
let arr2: String = "foo";

print_str(arr2);  // result: "foo"

arr2[0] = 'b';
arr2[1] = 'a';
arr2[2] = 'r';

print_str(arr2);  // result: "bar"

/* ------------------------ */
/*           loops          */
/* ------------------------ */

let sum: Int = 0;

for i: Int in 1..10 {
    sum = sum + i;    
}

print(sum);  // result: 55

while sum > 0 {
    sum = sum - 10;
}

print(sum);  // result: -5

/* ------------------------ */
/*        conditions        */
/* ------------------------ */

if sum == -5 {
    print_str("success!");
} else {
    print_str("fail...");
}

if false {
    print_str("fail...");
} else {
    print_str("success!");
}