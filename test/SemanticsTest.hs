{-# LANGUAGE TemplateHaskell #-}
module SemanticsTest where

import Test.QuickCheck
import Runner

-- fibonacchi function
fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

-- tests the iterative fib function
fibTestIter :: Integer -> IO ()
fibTestIter arg = putStrLn ("Running: \"fib_iter (" ++ show arg ++ ")\" \n Expected: " ++ show (fib arg)) *> runFileFun "demo/fib.as" "fib_iter" [arg]

-- tests the recursive fib function
fibTestRec :: Integer -> IO ()
fibTestRec arg = putStrLn ("Running: \"fib_rec (" ++ show arg ++ ")\" \n Expected: " ++ show (fib arg)) *> runFileFun "demo/fib.as" "fib_rec" [arg]

-- tests the functions in functions.as
funTestFive :: IO ()
funTestFive = putStrLn "Running: \"five ()\" \n Expected: 7, 2, 3, 5" *> runFile "demo/functions.as"

-- tests the concurrency in banking.as
funTestParallel :: IO ()
funTestParallel = putStrLn "Running: \"parallels.as\" \n Expected: 20000" *> runFile "demo/banking.as"


-- runs all the tests
runSemanticsTests :: IO ()
runSemanticsTests =
    putStrLn "Running SemanticsTests.hs..." *>
    putStrLn "demo/fib.as: " *>
    foldr (*>) (pure ()) (map fibTestIter [1..5]) *>
    putStrLn "Running fib_rec tests..." *>
    foldr (*>) (pure ()) (map fibTestRec [1..5]) *>
    putStrLn "demo/functions.as: " *>
    funTestFive *>
    putStrLn "demo/parallels.as: " *>
    funTestParallel *>
    putStrLn "SemanticsTests.hs finished."


