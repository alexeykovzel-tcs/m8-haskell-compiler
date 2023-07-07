/* 
    This file contains a basic mathematical library with support 
    for division, exponentiation, etc.
*/

// increments a value
fun incr(a: Int) -> Int {
    return a + 1;
}

// calculates a quotient after division 
fun div(a: Int, b: Int) -> Int {
    if b <= 0 {
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

/* --- uncomment for testing --- */

// print(incr(402));       // result: 403 
// print(div(100, 3));     // result: 33
// print(mod(500, 30));    // result: 20
// print(mod(500, -1));    // result: -1 (error)
// print(pow(4, 4));       // result: 256
// print(abs(-55));        // result: 55