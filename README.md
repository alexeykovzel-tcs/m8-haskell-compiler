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

## Source files: /src

| file            | description                                                    |
| --------------- | -------------------------------------------------------------- |
| Compiler.hs     | compiles AST into the SpriL language                           |
| Elaborator.hs   | handles errors during compilation                              |
| Lexer.hs        | defines language tokens & basic parsers                        |
| Parser.hs       | parses the source code into AST                                |
| PreCompiler.hs  | collects information about the program before compilation      |
| Runner.hs       | runs/debugs programs, and prints compiled instructions         |
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

You can try some of the already written programs in the /demo directory:

| file         | description                                                                                                         |
| ------------ | ------------------------------------------------------------------------------------------------------------------- |
| banking.as   | demonstrates the functionality for parallel execution and synchronization using the locking mechanism               |
| basic.as     | demonstrates the basic functionality of the compiler                                                                |
| fib.as       | contains algorithms for the fibonacci sequence                                                                      |
| functions.as | demonstrates the functionality of nested functions as well as nested variables                                      |
| math.as      | contains a basic mathematical library with support for division, exponentiation, etc                                |
| parallels.as | demonstrates the functionality for the nested parallel execution                                                    |
| peterson.as  | contains an implementation for the Peterson' algorithm                                                              |
| scopes.as    | demonstrates a fairly complex example of nested scoping with variables having the same name but in different scopes |

To try specific functions from the prebuilt modules in /demo, use:

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