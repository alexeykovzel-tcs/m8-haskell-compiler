module Main where

import Sprockell
import Compiler (compile, compileRun)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Control.Monad (join)
import Runner

{- 
    You can either pass a file path as an argument, in which case the
    corresponding file gets executed, or you can test the compiler using
    prebuilt modules. The following modules are available:

    fib:     fib_rec, fib_iter
    math:    incr, div, mod, pow, abs

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
    putStrLn "\nEnter: {module} {function} {args} or quit"
    line <- getLine
    handleLine line

handleLine :: String -> IO()
handleLine "quit" = pure ()
handleLine line = do
    let parts     = words line  
    let filePath  = "demo/" ++ parts !! 0 ++ ".txt"
    let funName   = parts !! 1
    let args      = read <$> drop 2 parts

    fileExists <- doesFileExist filePath
    
    if fileExists
        then runFun filePath funName args
        else putStrLn $ "File " ++ filePath ++ " does not exist."

    main

runFun :: FilePath -> String -> [Integer] -> IO() 
runFun file name args = join 
    $ (\code -> run [compileRun code name args]) 
    <$> readFile file 