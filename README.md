## How to use?

Run the following command:

```bash
stack ghci src/Runner.hs
```

Then, you can have following options to test the compiler:

```haskell

-- run program in a file
runFile "demo/fib.txt"

-- run program as a string
runProg "for i in 1..3 { print (i); }" 

```

where program is \[Instruction\]

## Parser usage

To test parser manually, run:

```bash
ghci -i:src src/Parser.hs
```

Then, use can try the following:

```haskell
tryParse statement "let x: Int = 2 + 1";
tryParse statement "for i: Int in [1, 2, 3] { print(i); }"
tryParse expr "2 * (3 + 4) / 6"
```