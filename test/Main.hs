module Main where

import SyntaxTest
import ContextualTest

main :: IO ()
main = do
  results <- sequence [
        SyntaxTest.check,
        ContextualTest.check
    ]
  if all id results
    then putStrLn "All tests passed"
    else putStrLn "Some tests failed"