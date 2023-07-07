module Main where

import SyntaxTest

main :: IO ()
main = do
  results <- sequence [
        SyntaxTest.check
    ]
  if all id results
    then putStrLn "All tests passed"
    else putStrLn "Some tests failed"