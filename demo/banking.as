/*
    This file demonstrates the functionality for parallel execution
    and synchronization using the locking mechanism.
    
    It creates 4 processes that make 1000 transactions each to 
    the same bank account. The expected result is 20000.
*/

global account: Int = 0;
global lock: Int;

parallel 4 {
    for i: Int in 1..1000 {
        lock(lock);
        account = account + 5;
        unlock(lock);
    }
}

print(account); // result: 20000