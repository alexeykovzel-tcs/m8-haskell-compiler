{-# LANGUAGE TemplateHaskell #-}
module SemanticsTests where

import Test.QuickCheck
import Compiler
import Runner


fib :: Integer -> Integer
fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

fib_test :: Integer -> IO ()
fib_test arg = putStrLn ("After running function \"fib_iter\" from demo/fib.as with argument " 
                        ++ show arg ++ " the expected result from Sprockell is " ++ show (fib arg) ++ ".") *> runFileFun "demo/fib.as" "fib_iter" [arg]
