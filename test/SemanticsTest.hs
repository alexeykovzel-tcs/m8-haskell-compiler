{-# LANGUAGE TemplateHaskell #-}
module SemanticsTests where

import Test.QuickCheck
import Runner

nums = [1..5]

fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

fibTestIter :: Integer -> IO ()
fibTestIter arg = putStrLn ("Running: \"fib_iter (" ++ show arg ++ ")\" \n Expected: " ++ show (fib arg)) *> runFileFun "demo/fib.as" "fib_iter" [arg]

fibTestRec :: Integer -> IO ()
fibTestRec arg = putStrLn ("Running: \"fib_rec (" ++ show arg ++ ")\" \n Expected: " ++ show (fib arg)) *> runFileFun "demo/fib.as" "fib_rec" [arg]

funTestFive :: IO ()
funTestFive = putStrLn "Running: \"five ()\" \n Expected: 2, 3, 5" *> runFileFun "demo/functions.as" "five" []

funTestParallel :: IO ()
funTestParallel = putStrLn "Running: \"parallels.as\" \n Expected: 20000" *> runFile "demo/parallels.as"

runSemanticsTests :: IO ()
runSemanticsTests =
    putStrLn "Running SemanticsTests.hs..." *>
    putStrLn "demo/fib.as: " *>
    foldr (*>) (pure ()) (map fibTestIter nums) *>
    putStrLn "Running fib_rec tests..." *>
    foldr (*>) (pure ()) (map fibTestRec nums) *>
    putStrLn "demo/functions.as: " *>
    funTestFive *>
    putStrLn "demo/parallels.as: " *>
    funTestParallel *>
    putStrLn "SemanticsTests.hs finished."


