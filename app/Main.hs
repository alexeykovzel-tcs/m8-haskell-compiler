module Main where

import Sprockell
import Compiler
import Runner

import System.Directory (doesFileExist)
import System.Environment (getArgs)

{- 
    the user has the following options:
    
    1. pass a file path as an argument, so that the 
    corresponding file gets executed.
    
    2. don't pass anything, so that the user can try function 
    from the prebuilt modules. The following modules are available:

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

-- runs functions in modules given by the user
runModules :: IO()
runModules = do
    putStrLn "\nEnter: {module} {function} {args} or quit"
    line <- getLine
    handleInput line

-- handles user input from the terminal 
handleInput :: String -> IO()
handleInput "quit" = pure ()
handleInput line = do
    let parts     = words line  
    let filePath  = "demo/" ++ parts !! 0 ++ ".txt"
    let funName   = parts !! 1
    let args      = read <$> drop 2 parts

    fileExists <- doesFileExist filePath
    
    if fileExists
        then runFileFun filePath funName args
        else putStrLn $ "File " ++ filePath ++ " does not exist."

    main