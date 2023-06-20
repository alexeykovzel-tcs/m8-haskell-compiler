## Parser usage

To test parser manually, run:

```bash
ghci -i:src src/Main.hs
```

Then, use can try the following:

```bash
tryParse statement "let x: Int = 2 + 1";
tryParse statement "for i: Int in [1, 2, 3] { print(i); }"
tryParse expr "2 * (3 + 4) / 6"
```