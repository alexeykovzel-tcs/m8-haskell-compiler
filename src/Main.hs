module Main where

import Text.ParserCombinators.Parsec
import Data.Either
import Parser


main :: IO()
main = return() 


example =
    "   fun fib(num: Int) -> Int {      " ++
    "       let a: Int = 1;             " ++
    "       let b: Int = 0;             " ++
    "       let temp: Int;              " ++
    "       while num >= 0 {            " ++
    "           temp = a;               " ++
    "           a = a + b;              " ++
    "           b = temp;               " ++
    "           num = num - 1;          " ++
    "       }                           " ++
    "       return b;                   " ++
    "   }                               " ++
    "                                   " ++
    "   fun main() -> None {            " ++
    "       print(\"fib series:\");     " ++
    "       print(fib(10));             " ++
    "   }                               "