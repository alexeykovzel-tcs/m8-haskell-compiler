module Elaborator where

import Parser (Script, Statement)
import Data.Maybe
import qualified Data.Map as Map

data Context
    = Scope [Context]
    | LUT (Map.Map String Attr)

data Attr = 
    | Variable  Type
    | Function
    | Struct

data Type =
    | String
    | Array Type
    | Integer
    | Boolean

data Error 
    = TypeError String

check :: Script -> Context
check program = checkScript (Scope []) program

checkScript :: Context -> Script -> Context
checkScript [x:xs] = check (checkStmt x) xs

checkStmt :: Context -> Statement -> Either Error Context

-- Not already declared
-- No type -> Has value (inferred type)
-- Has Type -> matches value
checkStmt ctx (VarDecl def@(VarDef name type) expr)
    | declared ctx name = Left $ TypeError $ "Tried to declare a variable twice: " ++ name
    | isNothing type && isNothing expr = Left $ TypeError $ "..."
    | isNothing type && isJust expr = -- infer type
    | otherwise = Right $ insertLUT ctx name

-- Variable is declared
-- Value matches type
checkStmt ctx (VarAssign name expr)
    | declared ctx name = Right ctx
    | otherwise = Left $ TypeError 
        $ "Missing variable declaration" ++ name

-- type is array
-- matches value
checkStmt ctx (ArrInsert name idx expr)

-- increase score
-- check script
-- add to LUT
-- check if script contains return ??
checkStmt ctx (FunDef name args returnType script) = 

-- add to LUT
checkStmt ctx (StructDef name args) = 

-- check iter types
-- check script types
-- add idx to LUT
checkStmt ctx (ForLoop idx iter script) = 

-- check that expr is bool
-- check script
checkStmt ctx (WhileLoop expr script) = 

-- check expr is bool
checkStmt ctx (Condition expr ifScript elseScript) = 

-- check if function return matches expr
checkStmt ctx (ReturnVal expr) = 

-- check expr    
checkStmt ctx (Action expr) = 

declared :: Context -> String -> Bool
declared = ...

insertLUT :: Context -> String -> Context
insertLUT = ...