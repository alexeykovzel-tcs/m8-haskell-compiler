{-# LANGUAGE TemplateHaskell #-}

module ContextualTest where

import Test.QuickCheck
import Elaborator
import Parser

-----------------------------------------------------------------------------
-- utils
-----------------------------------------------------------------------------

instance Eq Error where
    (==) (InvalidType _ _) (InvalidType _ _) = True
    (==) (InvalidExprType _ _) (InvalidExprType _ _) = True
    (==) (DupDecl _) (DupDecl _) = True
    (==) (MissingDecl _) (MissingDecl _) = True
    (==) (NotAssigned _) (NotAssigned _) = True
    (==) (GlobalDecl _) (GlobalDecl _) = True
    (==) _ _ = False

errors :: TypeChecker -> [Error]
errors (TypeChecker errs _) = errs

elaborateErrors :: String -> [Error]
elaborateErrors = errors . tryElaborate . parseScript

catch :: String -> Error
catch = head . elaborateErrors

wrongType :: Error
wrongType = InvalidType ("somevar", IntType, True, False, -1) BoolType

-----------------------------------------------------------------------------
-- incorrect scripts
-----------------------------------------------------------------------------

prop_wrongType1 = wrongType == catch "let var1: Int = 1; var1 = true;"
prop_wrongType2 = wrongType == catch "let var1: Bool = false; var1 = 2;"
prop_wrongType3 = wrongType == catch "let var1: Int = 1; let var2: Int[4] = [20,21,22,23]; var2[0] = true;"

prop_missingDecl1 = MissingDecl "var2" == catch  "let var1: Int = 1; var2 = 2;"
prop_missingDecl2 = MissingDecl "x" == catch "let var1: Int = 5; x = var1 + 1;"
prop_missingDecl3 = MissingDecl "x" == catch "let flag: Bool = true; notflag = x == 5;"

prop_notAssigned = NotAssigned "var1" == catch "let var1: Int; let var2: Int = var1;"
prop_globalDecl = GlobalDecl "var2" == catch "global var1: Int = 1; for i: Int in 0..10 { global var2: Int = 2; }"
prop_dupDecl = DupDecl "var1" == catch "let var1: Int = 1; let var1: Int = 2;"

prop_invalidExprType = InvalidExprType BoolType IntType == catch "let var1: Int = 1; if var1 { print(var1); };"

-----------------------------------------------------------------------------
-- correct scripts
-----------------------------------------------------------------------------

prop_correct1 = [] ==  elaborateErrors "let var1: Int = 1; var1 = 2;"
prop_correct2 = [] == elaborateErrors "let var1: Bool = true; let var3: Int = 3; var1 = false; var3 = 4;"
prop_correct3 = [] == elaborateErrors "global var1: Int = 1; for i: Int in 0..10 { var1 = var1 + 1; }"
prop_correct4 = [] == elaborateErrors "let var1: Int = 1; let arr: Int[4] = [20,21,22,23]; arr[0] = 1;"
prop_correct5 = [] == elaborateErrors "let flag: Bool = true; parallel 2 { if flag { print(1); } else { print(2); } }"
prop_correct6 = [] == elaborateErrors "global var1: Int = 1; while var1 < 10 { var1 = var1 + 1; }"
prop_correct7 = [] == elaborateErrors "fun incr(x: Int) -> Int { return x + 1; }"
prop_correct8 = [] == elaborateErrors "fun fib(x: Int) -> Int { if x < 2 { return x; } else { return fib(x - 1) + fib(x - 2); } }"
prop_correct9 = [] == elaborateErrors "fun parallelFunction() -> Int { parallel 5 { print(1); } return 0; }"
prop_correct10 = [] == elaborateErrors "global var: Int = 0; fun par(x: Int) -> Int {if x < 2 { return x; } else { for i: Int in 0..2 { while var < 10 { var = var + 1; } } return par(x - 1) + par(x - 2); } }"

return []
check = $quickCheckAll