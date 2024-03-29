/*
    This file demonstrates the functionality of nested functions
    as well as nested variables.
*/

fun two() -> Int {
    return 2;
}

let x: Int = 7;

fun five() -> Int {
    let x: Int = 0;

    fun three() -> Int {
        return 3;
    }

    if true {         
        print(two());   // 2
        print(three()); // 3

        x = two() + three();
    }

    return x;
}

print(x); // 7

x = five();

print(x); // 5