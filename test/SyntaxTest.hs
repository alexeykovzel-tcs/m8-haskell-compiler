{-# LANGUAGE TemplateHaskell #-}

module SyntaxTest where

import Test.QuickCheck as QC
import Control.Exception
import Parser
import Lexer

-----------------------------------------------------------------------------
-- generator for the correct script
-----------------------------------------------------------------------------

instance QC.Arbitrary Statement where
    arbitrary = QC.oneof [
        VarDecl     <$> QC.arbitrary <*> QC.arbitrary,
        GlVarDecl   <$> QC.arbitrary <*> QC.arbitrary,
        VarAssign   <$> QC.arbitrary <*> QC.arbitrary,
        ArrInsert   <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        FunDef      <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        ForLoop     <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        WhileLoop   <$> QC.arbitrary <*> QC.arbitrary,
        Condition   <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        Parallel    <$> QC.arbitrary <*> QC.arbitrary,
        InScope     <$> QC.arbitrary,
        ReturnVal   <$> QC.arbitrary,
        Action      <$> QC.arbitrary ]

instance QC.Arbitrary Expr where
    arbitrary = QC.sized expr
        where
            expr 0 = Parser.Fixed <$> QC.arbitrary
            expr n = let nextExpr = expr (n `div` 2) in QC.oneof [
                FunCall         <$> QC.arbitrary <*> QC.resize (n `div` 2) QC.arbitrary,
                Ternary         <$> nextExpr <*> nextExpr <*> nextExpr,
                ArrAccess       <$> QC.arbitrary <*> QC.arbitrary,
                Both            <$> nextExpr <*> nextExpr,
                OneOf           <$> nextExpr <*> nextExpr,
                Eq              <$> nextExpr <*> nextExpr,
                MoreEq          <$> nextExpr <*> nextExpr,
                LessEq          <$> nextExpr <*> nextExpr,
                More            <$> nextExpr <*> nextExpr,
                Less            <$> nextExpr <*> nextExpr,
                Add             <$> nextExpr <*> nextExpr,
                Sub             <$> nextExpr <*> nextExpr,
                Mult            <$> nextExpr <*> nextExpr,
                Neg             <$> nextExpr,
                Var             <$> QC.arbitrary,
                Parser.Fixed    <$> QC.arbitrary ]

instance QC.Arbitrary DataType where
    arbitrary = QC.oneof [
        pure CharType,
        pure BoolType,
        pure IntType,
        ArrType <$> QC.arbitrary <*> QC.arbitrary ]

instance QC.Arbitrary Value where
    arbitrary = QC.oneof [
        Bool <$> QC.arbitrary,
        Char <$> QC.arbitrary,
        Int  <$> QC.arbitrary,
        Arr  <$> QC.arbitrary]

instance QC.Arbitrary LoopIter where
    arbitrary = IterRange <$> QC.arbitrary <*> QC.arbitrary

autoScript :: IO Script
autoScript = QC.generate (QC.resize 3 QC.arbitrary)

-----------------------------------------------------------------------------
-- utils
-----------------------------------------------------------------------------

-- a helper function to check if an error is thrown.
throwsError :: a -> IO Bool
throwsError x = catch (evaluate x >> return False) handler

handler :: ErrorCall -> IO Bool
handler _ = return True

-- invalid test inputs
testInvalidInputs :: [String]
testInvalidInputs = 
    [
        "let x = 1;", -- VarDecl
        "let x Int = 1;", -- VarDecl
        "let x: Int = ;", -- VarDecl
        "global x = 1;", -- GlVarDecl
        "global x Int = 1;", -- GlVarDecl
        "global x: Int = ;", -- GlVarDecl
        "x  1;", -- VarAssign
        "x -> 1;", -- VarAssign
        "x = ;", -- VarAssign
        "x[1] -> 1;", -- ArrInsert
        "x[1] = ;", -- ArrInsert
        "x[1] : Int = 1;", -- ArrInsert
        "x[1] : 1;", -- ArrInsert
        "def f() -> Int { return 1; }", -- FunDef
        "fun f() -> x { return 1; }", -- FunDef
        "fun f -> Int { return 1; }", -- FunDef
        "for x in 1..10 { return 1; }", -- ForLoop
        "for x: Int in 1.. { return 1; }", -- ForLoop
        "for x in 1.. { return 1; }", -- ForLoop
        "while (x  3) { return 1; }", -- WhileLoop
        "while x: Int { return 1; }", -- WhileLoop
        "if (x  3) { return 1; }", -- Condition
        "if x: Int { return 1; }", -- Condition
        "par { return 1; }", -- Parallel
        "parallel { return 1; }", -- Parallel
        "return x: Int;", -- ReturnVal
        "print(Int);", -- Action
        "let x: Int = 1; print(x); parallel { return 1; }", -- Script
        "let x = 1, print(x);",
        "let x = 1 print(x)",
        "print(;",
        "let x = ; print(x)",
        "let x = 1; let x = 2;",
        "let x = 2; let y = x + ;",
        "let x = 3; if (x >) { print(x); }",
        "let x = 4; while (x >) { print(x); }",
        "let x = 5; for (x in..31) { print(x); }",
        "let x = 6; def f() { return x +; }",
        "let x = 7; def f(x) { return x +; }",
        "let x = 8; def f() { return ; }"
    ]

-- valid test inputs
testValidInputs :: [String]
testValidInputs = 
    [
        "let x: Int = 1;", -- VarDecl
        "global x: Int = 1;", -- GlVarDecl
        "x = 1;", -- VarAssign
        "x[3] = 1;", -- ArrInsert
        "fun f() -> Int { return 1; }", -- FunDef
        "fun f() { return 1; }", -- FunDef
        "for x: Int in 1..10 { return 1; }", -- ForLoop
        "while x > 3 { return 1; }", -- WhileLoop
        "if x > 3 { return 1; }", -- Condition
        "if x > 3 { return 1; } else { return 2; }", -- Condition
        "parallel 2 { x = x + 4; }", -- Parallel
        "return 1;", -- ReturnVal
        "print(x);", -- Action
        "let x: Int = 1; print(x); parallel 2 { return 1; }", -- Script
        "let x: Int = 1; print(x);", -- Script
        "if x > 3 { return 1; } else {parallel 2 { return 2; }}", -- Script
        "let x: Int = 1; if x > 3 { return 1; } else { parallel 2 { return 2; }}", -- Script
        "for x: Int in 1..10 { return 1; } if x > 3 { return 1; } else {if x > 2 {parallel 2 { return 2; }}}" -- Script
    ]

-- test that error thrown when parsing invalid syntax with one element
prop_invalidSyntax :: Property
prop_invalidSyntax =
  forAll (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) $ \s ->
    ioProperty $ throwsError $ parseWith script s

-- test that error thrown when parsing invalid syntax of script
prop_invalidSyntax2 :: Property
prop_invalidSyntax2 = forAll (elements testInvalidInputs) $ \s ->
    ioProperty $ throwsError $ parseWith script s

prop_validSyntax :: Property
prop_validSyntax = forAll (elements testValidInputs) $ \s ->
    ioProperty $ not <$> throwsError (parseWith script s)

return []
check = $quickCheckAll