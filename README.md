# PP Final Project: AlphaScript

This project ... bla bla bla

## Prerequisites

Make sure you have installed:

- Stack (<https://docs.haskellstack.org/en/stable/README/>). Version needs to be 2.7 or higher.

To test if you have these tools set up properly, run the following command in the command line:

```bash
stack --version
```

## Compiling

In a terminal, run:

```bash
stack build
```

This installs a local version of GHC and the libraries needed. The files in `src/` and `app/` are then compiled.

## Running

To run a specific file, use:

```bash
stack run -- {file_path}
```

To try functions of the prebuilt modules in /demo, use:

```bash
stack run
```

The following modules are available:

| module | description                   | functions                                 |
| ------ | ----------------------------- | ----------------------------------------- |
| fib    | fibonacci sequence algorithms | fib_rec (recursive), fib_iter (iterative) |
| math   | basic math library            | incr, div, mod, pow, abs                  |

For example:

```haskell
math div 100 5      -- results in 20
fib fib_iter 10     -- results in 55
```

## Tests

In a terminal, run:

```
stack test
```

This will run all test executables as defined in the "test" section of `package.yml`.
