module Main where

import SyntaxTest
import ElaboratorTest

main :: IO ()
main = do
  results <- sequence [
        SyntaxTest.check,
        ElaboratorTest.check
    ]
  if all id results
    then putStrLn "All tests passed"
    else putStrLn "Some tests failed"