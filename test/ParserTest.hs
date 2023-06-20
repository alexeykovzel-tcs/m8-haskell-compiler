module Test.ParserTest where

import Test.QuickCheck
import Parser

prop_int x  = parser value (show x) == Int x
prop_str    = parser value "\"Hello\"" == Str "Hello"

runTests :: IO ()
runTests = do

  putStrLn "\nRunning prop_int:"
  quickCheck prop_int

  putStrLn "\nRunning prop_str:"
  quickCheck prop_str

  putStrLn ""