module Main where

import Sprockell
import Compiler
import Runner

import System.Directory (doesFileExist)
import System.Environment (getArgs)

{- 
    the user has the following options to test the compiler:
    
    1. pass a file path as an argument: "stack run -- demo/math.txt",
       and the corresponding file gets executed.
    
    2. don't pass anything, and the user will be able to try functions 
       from the prebuilt modules. 
       
    The following modules are available:

    - fib    | fib_rec, fib_iter
    - math   | incr, div, mod, pow, abs

    For example, here are possible commands:

    math div 100 5      // result: 20
    fib fib_iter 10     // result: 55
-}

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath]  -> runFile filePath
        []          -> runModules

runModules :: IO()
runModules = do
    putStrLn "\nthe following modules are available:\n"
    
    putStrLn "fib    | fib_rec (recursive), fib_iter (iterative)"
    putStrLn "math   | incr, div, mod, pow, abs"
    
    putStrLn "\nfor example, here are possible commands:\n"
    
    putStrLn "math div 100 5     // result: 20"
    putStrLn "fib fib_iter 10    // result: 55"

    askInputCycle

askInputCycle :: IO()
askInputCycle = do
    putStrLn "\nenter: {module} {function} {args} or quit"
    line <- getLine
    handleInput line

handleInput :: String -> IO()
handleInput "quit" = pure ()
handleInput line = do
    let parts     = words line  
    let filePath  = "demo/" ++ parts !! 0 ++ ".as"
    let funName   = parts !! 1
    let args      = read <$> drop 2 parts

    fileExists <- doesFileExist filePath
    
    if fileExists
        then runFileFun filePath funName args
        else putStrLn $ "file " ++ filePath ++ " does not exist."

    askInputCycle