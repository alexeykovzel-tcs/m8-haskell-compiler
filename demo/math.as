/* 
    This file contains a basic mathematical library with support 
    for division, exponentiation, etc.
*/

// calculates a factorial
fun factorial(a: Int) -> Int {
    if a < 0 {
        error("value shouldn't be negative");
    }
    let k: Int = 1;
    for i: Int in 1..a {
        k = k * i;
    }
    return k;
}

// increments a value
fun incr(a: Int) -> Int {
    return a + 1;
}

// decrements a value
fun decr(a: Int) -> Int {
    return a - 1;   
}

// calculates a quotient after division 
fun div(a: Int, b: Int) -> Int {
    if b <= 0 || b > a {
        error("invalid dividend");
    }
    let k: Int = 0;
    while a >= b {
        a = a - b;
        k = k + 1;
    }
    return k;
}

// calculates a remainder after division
fun mod(a: Int, b: Int) -> Int {
    if b <= 0 {
        error("invalid dividend");
    }
    while a >= b {
        a = a - b;
    }
    return a;
}

// raises a value to the given power
fun pow(a: Int, b: Int) -> Int {
    if b < 0 {
        error("power shouldn't be negative");
    }
    let k: Int = 1;
    for i: Int in 1..b {
        k = k * a;
    }
    return k;
}

// retrieves the absolute value
fun abs(a: Int) -> Int {
    return a >= 0 ? a : -a;
}

// calculates the greatest common divisor
fun gcd(a: Int, b: Int) -> Int {
    while b != 0 {
        let k: Int = b;
        b = mod(a, b);
        a = k;
    }
    return a;
}

// calculates the least common multiple
fun lcm(a: Int, b: Int) -> Int {
    return div(abs(a * b), gcd(a, b));
}

/* --- uncomment for testing --- */

// print(factorial(10));    // result: 3628800
// print(incr(402));        // result: 403 
// print(div(100, 3));      // result: 33
// print(mod(500, 30));     // result: 20
// print(pow(4, 4));        // result: 256
// print(abs(-55));         // result: 55
// print(gcd(266, 21));     // result: 7
// print(lcm(18, 26));      // result: 234