/*
    This file demonstrates the functionality for 
    the nested parallel execution.
*/

parallel 3 {
    print(0);
}
parallel 2 {
    parallel 2 {
        print(1);
    }
    parallel 2 {
        print(2);
    }
    parallel 2 {
        print(3);
    }
}