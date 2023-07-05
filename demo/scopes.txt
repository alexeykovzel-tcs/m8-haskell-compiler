/*
    This file demonstrates a fairly complex example of nested scoping
    with variables having the same name but in different scopes. 
*/

let x: Int = 1;
let y: Int = 2;
let z: Int = 3;

print_str("-----------------");

print(x); // result: 1
print(y); // result: 2
print(z); // result: 3

print_str("-----------------");

// 1st iter: 2, 3, 4, 5, 4
// 2nd iter: 4, 5, 6, 5, 4, 6, 7, 6, 5, 4

for i: Int in 1..2 {
    let y: Int = i * 2;
    print(y);

    for j: Int in 1..2 {
        let z: Int = j + y;
        print(z);

        if z > 3 {
            let x: Int = z + 1;
            print(x);

            while x > 4 {
                let y: Int = x - 1;
                print(y);
                x = x - 1;
            }
        }
    }
    
    print_str("-----------------");
}

print(x); // result: 1
print(y); // result: 2
print(z); // result: 3

print_str("-----------------");