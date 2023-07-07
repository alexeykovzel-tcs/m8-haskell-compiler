import Elaborator
import Parser


-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

instance Eq Error where
    (==) (InvalidType _ _) (InvalidType _ _) = True
    (==) (InvalidExprType _ _) (InvalidExprType _ _) = True
    (==) (DupDecl _) (DupDecl _) = True
    (==) (MissingDecl _) (MissingDecl _) = True
    (==) (NotAssigned _) (NotAssigned _) = True
    (==) (GlobalDecl _) (GlobalDecl _) = True
    (==) (NoReturn _) (NoReturn _) = True
    (==) _ _ = False

-- assert function
assertEqual :: (Eq a, Show a) => String -> a -> a -> String -> IO ()
assertEqual msg expected actual code = 
    if expected == actual 
    then putStrLn (msg ++ " Passed!" ++ " for code: " ++ code) 
    else putStrLn (msg ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show actual ++ " for code: " ++ code)

-- Your own 'errors' function to extract the errors from the TypeChecker
errors :: TypeChecker -> [Error]
errors (TypeChecker errs _) = errs

dupDecl = "let var1: Int = 1; let var1: Int = 2;" -- Duplicate declaration
unDecl = "let var1: Int = 1; var2 = 2;" -- Undeclared variable
wrongType = "let var1: Int = 1; var1 = true;" -- Wrong type
wrongType2 = "let var1: Bool = false; var1 = 2;" -- Wrong type
wrongType3 = "let var1: Int = 1; let var2: Int[4] = [20,21,22,23]; var2[0] = true;" -- Wrong type
wt = [wrongType, wrongType2, wrongType3]
wrongExprType = "let var1: Int = 1; if var1 { print(var1); };" -- Wrong expression type
missingDecl = "let var1: Int = 5; x = var1 + 1;" -- Missing declaration
missingDecl2 = "let flag: Bool = true; notflag = x == 5;" -- Missing declaration
md = [missingDecl, missingDecl2]
unAsgn = "let var1: Int; let var2: Int = var1;" -- Unassigned variable
globalDecl = "global var1: Int = 1; for i: Int in 0..10 { global var2: Int = 2; }" -- Global declaration
noReturn = "fun f1() -> Int { let var1: Int = 1; }" -- No return

-- Using the assert function
testDupDecl :: IO ()
testDupDecl = assertEqual "DupDecl error" (DupDecl "var1") (head $ errors $ tryElaborate $ parseWith script dupDecl) dupDecl

testUnDecl :: IO ()
testUnDecl = assertEqual "UnDecl error" (MissingDecl "var2") (head $ errors $ tryElaborate $ parseWith script unDecl) unDecl

-- wrong type test for every instance in one function
testWrongTypeAll :: IO ()
testWrongTypeAll = mapM_ testWrongType wt

testWrongType :: String -> IO ()
testWrongType str = assertEqual "WrongType error" (InvalidType ("somevar",IntType,True,False) BoolType) (head $ errors $ tryElaborate $ parseWith script str) str

testWrongExprType :: IO ()
testWrongExprType = assertEqual "WrongExprType error" (InvalidExprType BoolType IntType) (head $ errors $ tryElaborate $ parseWith script wrongExprType) wrongExprType

testMissingDeclAll :: IO ()
testMissingDeclAll = mapM_ testMissingDecl md 

testMissingDecl :: String -> IO ()
testMissingDecl str = assertEqual "MissingDecl error" (MissingDecl "x") (head $ errors $ tryElaborate $ parseWith script str) str

testUnAsgn :: IO ()
testUnAsgn = assertEqual "UnAsgn error" (NotAssigned "var1") (head $ errors $ tryElaborate $ parseWith script unAsgn) unAsgn

testGlobalDecl :: IO ()
testGlobalDecl = assertEqual "GlobalDecl error" (GlobalDecl "var2") (head $ errors $ tryElaborate $ parseWith script globalDecl) globalDecl

testNoReturn :: IO ()
testNoReturn = assertEqual "NoReturn error" (NotAssigned "var1" ) (head $ errors $ tryElaborate $ parseWith script noReturn) noReturn

runAllTests :: IO ()
runAllTests =
    testDupDecl *>
    putStrLn "" *>
    testUnDecl *>
    putStrLn "" *>
    testWrongTypeAll *>
    putStrLn "" *>
    testWrongExprType *>
    putStrLn "" *>
    testMissingDeclAll *>
    putStrLn "" *>
    testUnAsgn *>
    putStrLn "" *>
    testGlobalDecl *>
    putStrLn "" *>
    -- testNoReturn *>
    putStrLn "Done!" 
