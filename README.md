## How to use?

Run the following command:

```bash
stack ghci src/Runner.hs
```

Then, you can have following options:

```haskell
runFile "demo/fib.txt"                    -- run program in a file
runProg "for i in 1..3 { print (i); }"    -- run program as a string
```