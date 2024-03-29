/*
    This file contains an implementation for the Peterson's algorithm.
*/

global flag1: Bool = false;
global flag2: Bool = false;
global turn: Int;

let pid: Int;
set_process_id(pid);

if pid == 0 {
    while true {
        flag1 = true;
        turn = 1;
        while flag2 && turn == 1 {}
        print_str("P0 is in the critical section");
        flag1 = false;
        print_str("P0 is done");
    }
}

if pid == 1 {
    while true {
        flag2 = true;
        turn = 0;
        while flag1 && turn == 0 {}
        print_str("P1 is in the critical section");
        flag2 = false;
        print_str("P1 is done");
    }
}