# PP Final Project: AlphaScript

This project is a compiler for a simple programming language called AlphaScript. 

It supports the following features:

- basic data types (Int, Char, Bool)
- while and for loops
- if/else conditions
- locks and synchronization
- parallel execution
- arrays and strings
- nested functions
- soft division (in "math.as")

## Source files: /src

| file            | description                                                    |
| --------------- | -------------------------------------------------------------- |
| Compiler.hs     | compiles AST into the SpriL language                           |
| Elaborator.hs   | handles errors during compilation                              |
| Lexer.hs        | defines language tokens & basic parsers                        |
| Parser.hs       | parses the source code into AST                                |
| PreCompiler.hs  | collects information about the program before compilation      |
| Runner.hs       | runs/debugs the program, and prints its compiled code          |
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

| file         | description                                                                 |
| ------------ | --------------------------------------------------------------------------- |
| banking.as   | An elementary banking system, consisting of several processes               |
| basic.as     | demonstrates the basic functionality of the compiler                        |
| fib.as       | algorithms for the fibonacci sequence                                       |
| functions.as | an example of nested functions as well as nested variables                  |
| math.as      | a basic mathematical library with support for division, exponentiation, etc |
| parallels.as | an example of the nested parallel execution                                 |
| peterson.as  | an implementation for the Peterson' algorithm                               |
| scopes.as    | a fairly complex example of nested scoping with variables                   |

To try specific functions from the prebuilt modules in /demo, use:

```bash
stack run
```

The following modules are available:

| module | description                   | functions                                           |
| ------ | ----------------------------- | --------------------------------------------------- |
| fib    | fibonacci sequence algorithms | fib_rec (recursive), fib_iter (iterative)           |
| math   | basic math library            | factorial, incr, decr, div, mod, pow, abs, gcd, lcm |

For example:

```haskell
math gcd 266 21     -- results in 7
fib fib_iter 10     -- results in 55
```

## Tests

In a terminal, run:

```
stack test
```

This will run all test executables as defined in the "test" section of `package.yml`.

In a terminal, run:
```
stack ghci test/SemanticsTest.hs
runSemanticsTests 
```

This will run all the semantics tests in defined in "test/Semantics.hs"