/*
    This file demonstrates the functionality of nested functions
    as well as nested variables.
*/

let x: Int = 7;

fun five() -> Int {
    let x: Int = 0;

    fun two() -> Int {
        return 2;
    }

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

let x: Int = five();

print(x); // 5