module Main where

import Test.Evaluator
import System.Environment (getArgs)
import Parser
import Syntax

fib :: String
fib = "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"

main :: IO ()
main = do
    args <- getArgs
    case args of

        (arg:_) -> do
            let x = read $ arg :: Integer
            let result = show $ eval fib x
            putStrLn $ "fibonacci " ++ show x ++ " is " ++ result
        
        [] -> putStrLn "Provide an integer"