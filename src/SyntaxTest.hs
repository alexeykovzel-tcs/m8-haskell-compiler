-- {-# LANGUAGE TemplateHaskell #-}

-- module SyntaxTest where

-- import Test.QuickCheck
-- import Control.Exception
-- import Parser
-- import Lexer

-- -----------------------------------------------------------------------------
-- -- generator for the correct script
-- -----------------------------------------------------------------------------

-- data NonEmptyList a = NonEmptyList [a] deriving (Show)

-- -- Generates non-empty scripts
-- instance Arbitrary a => Arbitrary (SyntaxTest.NonEmptyList a) where
--     arbitrary = SyntaxTest.NonEmptyList <$> (listOf1 arbitrary)

-- instance Arbitrary Statement where
--     arbitrary = oneof [
--         VarDecl    <$> arbitrary <*> arbitrary,
--         GlVarDecl  <$> arbitrary <*> arbitrary,
--         VarAssign  <$> arbitrary <*> arbitrary,
--         ArrInsert  <$> arbitrary <*> arbitrary <*> arbitrary,
--         FunDef     <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
--         ForLoop    <$> arbitrary <*> arbitrary <*> arbitrary,
--         WhileLoop  <$> arbitrary <*> arbitrary,
--         Condition  <$> arbitrary <*> arbitrary <*> arbitrary,
--         Parallel   <$> arbitrary <*> arbitrary,
--         InScope    <$> arbitrary,
--         ReturnVal  <$> arbitrary,
--         Action     <$> arbitrary ]

-- instance Arbitrary Expr where
--     arbitrary = sized expr
--         where
--             expr 0 = Parser.Fixed <$> arbitrary
--             expr n = let nextExpr = expr (n `div` 2) in oneof [
--                 FunCall    <$> arbitrary <*> resize (n `div` 2) arbitrary,
--                 ArrAccess  <$> arbitrary <*> arbitrary,
--                 Ternary    <$> nextExpr  <*> nextExpr <*> nextExpr,
--                 Both       <$> nextExpr  <*> nextExpr,
--                 OneOf      <$> nextExpr  <*> nextExpr,
--                 Eq         <$> nextExpr  <*> nextExpr,
--                 MoreEq     <$> nextExpr  <*> nextExpr,
--                 LessEq     <$> nextExpr  <*> nextExpr,
--                 More       <$> nextExpr  <*> nextExpr,
--                 Less       <$> nextExpr  <*> nextExpr,
--                 Add        <$> nextExpr  <*> nextExpr,
--                 Sub        <$> nextExpr  <*> nextExpr,
--                 Mult       <$> nextExpr  <*> nextExpr,
--                 Neg        <$> nextExpr,
--                 Var        <$> arbitrary,
--                 Parser.Fixed  <$> arbitrary ]

-- instance Arbitrary DataType where
--     arbitrary = oneof [
--         pure CharType,
--         pure BoolType,
--         pure IntType,
--         ArrType <$> arbitrary <*> arbitrary ]

-- instance Arbitrary Value where
--     arbitrary = oneof [
--         Bool <$> arbitrary,
--         Char <$> arbitrary,
--         Int  <$> arbitrary,
--         Arr  <$> arbitrary ]

-- instance Arbitrary LoopIter where
--     arbitrary = IterRange <$> arbitrary <*> arbitrary

-- prop_script :: Property
-- prop_script = forAll autoScript 
--         $ \(NonEmptyList script) -> parseScript (pretty script) == script


-- autoScript :: Gen (SyntaxTest.NonEmptyList Statement)
-- autoScript = NonEmptyList <$> resize 3 arbitrary

-- -----------------------------------------------------------------------------
-- -- utils
-- -----------------------------------------------------------------------------

-- -- a helper function to check if an error is thrown.
-- throwsError :: a -> IO Bool
-- throwsError x = catch (evaluate x >> return False) handler

-- handler :: ErrorCall -> IO Bool
-- handler _ = return True

-- -- invalid test inputs
-- invalidInputs :: [String]
-- invalidInputs = 
--     [
--         "let x = 1;", -- VarDecl
--         "let x Int = 1;", -- VarDecl
--         "let x: Int = ;", -- VarDecl
--         "global x = 1;", -- GlVarDecl
--         "global x Int = 1;", -- GlVarDecl
--         "global x: Int = ;", -- GlVarDecl
--         "x  1;", -- VarAssign
--         "x -> 1;", -- VarAssign
--         "x = ;", -- VarAssign
--         "x[1] -> 1;", -- ArrInsert
--         "x[1] = ;", -- ArrInsert
--         "x[1] : Int = 1;", -- ArrInsert
--         "x[1] : 1;", -- ArrInsert
--         "def f() -> Int { return 1; }", -- FunDef
--         "fun f() -> x { return 1; }", -- FunDef
--         "fun f -> Int { return 1; }", -- FunDef
--         "for x in 1..10 { return 1; }", -- ForLoop
--         "for x: Int in 1.. { return 1; }", -- ForLoop
--         "for x in 1.. { return 1; }", -- ForLoop
--         "while (x  3) { return 1; }", -- WhileLoop
--         "while x: Int { return 1; }", -- WhileLoop
--         "if (x  3) { return 1; }", -- Condition
--         "if x: Int { return 1; }", -- Condition
--         "par { return 1; }", -- Parallel
--         "parallel { return 1; }", -- Parallel
--         "return x: Int;", -- ReturnVal
--         "print(Int);", -- Action
--         "let x: Int = 1; print(x); parallel { return 1; }", -- Script
--         "let x = 1, print(x);",
--         "let x = 1 print(x)",
--         "print(;",
--         "let x = ; print(x)",
--         "let x = 1; let x = 2;",
--         "let x = 2; let y = x + ;",
--         "let x = 3; if (x >) { print(x); }",
--         "let x = 4; while (x >) { print(x); }",
--         "let x = 5; for (x in..31) { print(x); }",
--         "let x = 6; def f() { return x +; }",
--         "let x = 7; def f(x) { return x +; }",
--         "let x = 8; def f() { return ; }"
--     ]

-- validInputs :: [String]
-- validInputs = 
--     [
--         "let x: Int = 1;", -- VarDecl
--         "global x: Int = 1;", -- GlVarDecl
--         "x = 1;", -- VarAssign
--         "x[3] = 1;", -- ArrInsert
--         "fun f() -> Int { return 1; }", -- FunDef
--         "fun f() { return 1; }", -- FunDef
--         "for x: Int in 1..10 { return 1; }", -- ForLoop
--         "while x > 3 { return 1; }", -- WhileLoop
--         "if x > 3 { return 1; }", -- Condition
--         "if x > 3 { return 1; } else { return 2; }", -- Condition
--         "parallel 2 { x = x + 4; }", -- Parallel
--         "return 1;", -- ReturnVal
--         "print(x);", -- Action
--         "let x: Int = 1; print(x); parallel 2 { return 1; }", -- Script
--         "let x: Int = 1; print(x);", -- Script
--         "if x > 3 { return 1; } else {parallel 2 { return 2; }}", -- Script
--         "let x: Int = 1; if x > 3 { return 1; } else { parallel 2 { return 2; }}", -- Script
--         "for x: Int in 1..10 { return 1; } if x > 3 { return 1; } else {if x > 2 {parallel 2 { return 2; }}}" -- Script
--     ]

-- -- test that error thrown when parsing invalid syntax with one element
-- prop_invalidSyntax :: Property
-- prop_invalidSyntax =
--   forAll (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) $ \s ->
--     ioProperty $ throwsError $ parseWith script s

-- -- test that error thrown when parsing invalid syntax of script
-- prop_invalidSyntaxScript :: Property
-- prop_invalidSyntaxScript = forAll (elements invalidInputs) $ \s ->
--     ioProperty $ throwsError $ parseWith script s

-- -- test that no error will be thrown when given valid
-- prop_validSyntaxScript :: Property
-- prop_validSyntaxScript = forAll (elements validInputs) $ \s ->
--     ioProperty $ not <$> throwsError (parseWith script s)

-- return []
-- check = $quickCheckAll