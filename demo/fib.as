/*
    This file contains algorithms for the fibonacci sequence.

    Note that as there is a strict restriction on memory (32 blocks),
    the recursive approach might not work for n > 5.
*/

// iterative algorithm for the fibonacci sequence
fun fib_iter(n: Int) -> Int {
    let a: Int = 0;
    let b: Int = 1;
    let c: Int;

    while n >= 2 {
        c = a + b;
        a = b;
        b = c;
        n = n - 1;
    }
    return b;
}

// recursive algorithm for the fibonacci sequence
fun fib_rec(n: Int) -> Int {
    return n > 1
        ? fib_rec(n - 1) + fib_rec(n - 2)
        : n;
}