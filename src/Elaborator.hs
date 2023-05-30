module Elaborator where

import Parser
import Data.Maybe
import Data.Either
import qualified Data.Map as Map

-- Context contains the information about declared
-- variables, structures, functions, etc. 
-- Each scope knows about its upper (parent) scope
data Context 
    = Root LUT
    | Scope Context LUT

type LUT = Map.Map String Attr

data Attr 
    = Var DataType
    | Fun [DataType] DataType
    | Struct [DataType]

data Error 
    = InvalidType String
    | EmptyDecl String
    | OutOfBounds

elaborate :: Script -> Either Error Context
elaborate script = checkScript (Root Map.empty) script

checkScript :: Context -> Script -> Either Error Context
checkScript ctx (x:xs) = case (checkStmt ctx x) of
    Right nextCtx -> checkScript nextCtx xs
    Left err -> Left err

checkStmt :: Context -> Statement -> Either Error Context

-- incomplete example which might help: 

-- checkStmt ctx (VarDecl def@(VarDef name type) expr)
--     | declared ctx name = Left $ InvalidType name
--     | isNothing type && isNothing expr = Left $ EmptyDecl name
--     | otherwise = Right $ insert ctx (Var type) name
--     | isNothing type && isJust expr = insert ctx 

-- Check: not already declared
-- Check: no type -> has value (type is inferred)
-- Check: has type -> matches value
checkStmt ctx (VarDecl def@(VarDef name dataType) expr) = Right ctx

-- Check: variable is declared
-- Check: value matches type
checkStmt ctx (VarAssign name expr) = Right ctx

-- Check: type is array
-- Check: value matches type
checkStmt ctx (ArrInsert name idx expr) = Right ctx

-- Check: script contains return 
-- Do: create score
-- Do: elaborate script
-- Do: add to context
checkStmt ctx (FunDef name args returnType script) = Right ctx

-- add to LUT
checkStmt ctx (StructDef name args) = Right ctx

-- Check: iter type
-- Do: elaborate script
-- Do: add idx to LUT
checkStmt ctx (ForLoop idx iter script) = Right ctx

-- Check: expr is bool
-- Do: elaborate script
checkStmt ctx (WhileLoop expr script) = Right ctx

-- Check: expr is bool
checkStmt ctx (Condition expr ifScript elseScript) = Right ctx

-- Check: function return matches expr
checkStmt ctx (ReturnVal expr) = Right ctx

exprType :: Expr -> DataType
exprType expr = StrType

declared :: String -> Context -> Bool
declared name (Scope upper lut) 
    = Map.member name lut || declared name upper

insert :: String -> Attr -> Context -> Context
insert name attr (Scope upper lut)
    = Scope upper (Map.insert name attr lut)