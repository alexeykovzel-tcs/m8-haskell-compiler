## Parser

To test parser manually, run:

```bash
ghci -i:src src/Main.hs
```

Then, use can try the following:

```
parser statement "let x: Int = 2 + 1";

parser statement "for i: Int in [1, 2, 3] { print(i); }"

parser expr "2 * (3 + 4) / 6"
```