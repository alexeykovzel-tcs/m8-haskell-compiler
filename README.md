# PP Final Project: AlphaScript

This project is a compiler for a simple programming language called AlphaScript. 
It supports the following features:

- basic data types (Int, Char, Bool)
- while and for loops
- if/else conditions
- locks and synchronization
- parallel execution
- arrays and strings
- nested functions with call-by-reference
- soft division ("math.as" library)

## Source files (/src)

| file name       | description                                                    |
| --------------- | -------------------------------------------------------------- |
| Compiler.hs     | compiles AST into the SpriL language                           |
| Elaborator.hs   | handles errors during compilation                              |
| Lexer.hs        | defines language tokens & basic parsers                        |
| Parser.hs       | parses the program code into AST                               |
| PreCompiler.hs  | collects information about the program before compilation      |
| Runner.hs       | runs/debugs programs, and prints their SpriL instructions      |
| SprockellExt.hs | manages memory, registers, IO, variables, scopes and processes |

## Prerequisites

Make sure you have installed:

- Stack (<https://docs.haskellstack.org/en/stable/README/>). Version needs to be 2.7 or higher.

To test if you have these tools set up properly, run the following command in the command line:

```bash
stack --version
```

## Compilation

In a terminal, run:

```bash
stack build
```

This installs a local version of GHC and the libraries needed. 
The files in `src/` and `app/` are then compiled.

## Running

To compile and run a specific file, use:

```bash
stack run -- {file_path}
```

To try functions from the prebuilt modules in /demo, use:

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