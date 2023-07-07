{-# LANGUAGE TemplateHaskell #-}

module ContextualTest where

import Test.QuickCheck
import Elaborator
import Parser

instance Eq Error where
    (==) (InvalidType _ _) (InvalidType _ _) = True
    (==) (InvalidExprType _ _) (InvalidExprType _ _) = True
    (==) (DupDecl _) (DupDecl _) = True
    (==) (MissingDecl _) (MissingDecl _) = True
    (==) (NotAssigned _) (NotAssigned _) = True
    (==) (GlobalDecl _) (GlobalDecl _) = True
    (==) (NoReturn _) (NoReturn _) = True
    (==) _ _ = False

errors :: TypeChecker -> [Error]
errors (TypeChecker errs _) = errs

catch :: String -> Error
catch = head . errors . tryElaborate . parseScript

wrongType :: Error
wrongType = InvalidType ("somevar", IntType, True, False) BoolType

prop_wrongType1 = wrongType == catch "let var1: Int = 1; var1 = true;"
prop_wrongType2 = wrongType == catch "let var1: Bool = false; var1 = 2;"
prop_wrongType3 = wrongType == catch "let var1: Int = 1; let var2: Int[4] = [20,21,22,23]; var2[0] = true;"

prop_missingDecl1 = MissingDecl "var2" == catch  "let var1: Int = 1; var2 = 2;"
prop_missingDecl2 = MissingDecl "x" == catch "let var1: Int = 5; x = var1 + 1;"
prop_missingDecl3 = MissingDecl "x" == catch "let flag: Bool = true; notflag = x == 5;"

prop_notAssigned = NotAssigned "var1" == catch "let var1: Int; let var2: Int = var1;"
prop_globalDecl = GlobalDecl "var2" == catch "global var1: Int = 1; for i: Int in 0..10 { global var2: Int = 2; }"
prop_dupDecl = DupDecl "var1" == catch "let var1: Int = 1; let var1: Int = 2;"

-- prop_noReturn = NoReturn "var1" == catch "fun f1() -> Int { let var1: Int = 1; }"

prop_invalidExprType = InvalidExprType BoolType IntType 
    == catch "let var1: Int = 1; if var1 { print(var1); };"

return []
check = $quickCheckAll