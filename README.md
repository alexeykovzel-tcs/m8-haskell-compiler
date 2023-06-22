## Sprockell

To run a program in sprockell:

```haskell
run [program]
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