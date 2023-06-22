module Elaborator where

import Parser
import Data.Maybe
import Data.Either
import Text.Parsec.Pos
import qualified Data.Map as Map

-- Context contains the information about declared variables, structures, 
-- functions, etc. Each scope knows about its upper (parent) scope.
data Context 
    = Scope Context LUT
    | Null

type LUT = Map.Map String Entity

data Entity
    = Var DataType
    | Fun [DataType] DataType
    | Struct [DataType]

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | EmptyDecl     SourcePos String    -- variable decl. with no type and value
    | NoReturn      SourcePos           -- function decl. without return

elaborate :: Script -> Either Error Context
elaborate script = checkScript (Scope Null Map.empty) script

checkScript :: Context -> Script -> Either Error Context
checkScript ctx (x:xs) = case (checkStmt ctx x) of
    Right nextCtx -> checkScript nextCtx xs
    Left err -> Left err

-- incomplete example of checkStmt(), which might be helpful: 

-- checkStmt ctx (VarDecl def@(VarDef name type) expr)
--     | declared ctx name = Left $ InvalidType name
--     | isNothing type && isNothing expr = Left $ EmptyDecl name
--     | otherwise = Right $ update ctx (Var type) name

checkStmt :: Context -> Statement -> Either Error Context

-- Check: not already declared
-- Check: no type -> has value (type is inferred)
-- Check: has type -> matches value
checkStmt ctx stmt@(VarDecl def expr) = Right ctx

-- Check: variable is declared
-- Check: expr matches type
checkStmt ctx stmt@(VarAssign name expr) = Right ctx

-- Check: type is array
-- Check: expr matches array type
checkStmt ctx stmt@(ArrInsert name idx expr) = Right ctx

-- Check: script contains a return statement
-- Do: create a new scope in the context
-- Do: elaborate script
-- Do: add function to context (with args and return type)
checkStmt ctx stmt@(FunDef name args returnType script) = Right ctx

-- Do: Add struct to context (with its args)
checkStmt ctx stmt@(StructDef name args) = Right ctx

-- Check: iter type
-- Do: elaborate script
-- Do: add idx (as variable) to context
checkStmt ctx stmt@(ForLoop idx iter script) = Right ctx

-- Check: expr is bool
-- Do: elaborate script
checkStmt ctx stmt@(WhileLoop expr script) = Right ctx

-- Check: expr is bool
-- Do: elaborate if script
-- Do: elaborate else script (if exists)
checkStmt ctx stmt@(Condition expr ifScript elseScript) = Right ctx

-- Check: function return type matches expr
checkStmt ctx stmt@(ReturnVal expr) = Right ctx

-----------------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------------------

-- Returns true if an entity with such name 
-- existing in the context
declared :: String -> Context -> Bool
declared name Null = False
declared name (Scope upper lut) 
    = Map.member name lut || declared name upper

-- Add an entity to the context 
update :: String -> Entity -> Context -> Context
update name entity (Scope upper lut)
    = Scope upper (Map.insert name entity lut)