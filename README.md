## TODO

- [ ] Parse array types
- [ ] Get rid of "try" (backtracking)
- [ ] Support underscore in names
- [ ] Write documentation for parser


## Testing

To parser test manually, run:

```bash
ghci src/Main.hs -i/src
```

Then, use can try the following:

```
parser statement "let x: Int = 2 + 1";

parser statement "for i: Int in [1, 2, 3] { print(i); }"

parser array "[1, 2, 3]"

parser expr "2 * (3 + 4) / 6"
```