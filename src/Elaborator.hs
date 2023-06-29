{-# LANGUAGE FlexibleInstances #-}

module Elaborator (
    Depth, Offset,
    VarTable, VarPos, VarSize,
    ScopeID,
    varTable
) where

import Parser
import Data.Maybe
import Data.Either
import Text.Parsec.Pos
import Control.Monad (join)
import Table
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- position variables in memory
-----------------------------------------------------------------------------

-- builds a table with memory positions for variables by scope;
-- each position contains scope depth and offset from data pointer (DP)
varTable :: Script -> VarTable
varTable script = varTable
    where (_, _, _, varTable) = allocScript (0, 0, (1, 0), []) script

testVarMap :: FilePath -> IO()
testVarMap file = join $ 
    printTable . varTable . tryParse script <$> readFile file

-----------------------------------------------------------------------------

type Offset      = Int
type Depth       = Int

type ScopeID     = Int
type VarSize     = Integer
type VarPos      = (Depth, Offset)
type VarTable    = Table ScopeID VarName (VarPos, VarSize)
type VarCtx      = (ScopeID, ScopeID, VarPos, VarTable)

allocVars :: VarCtx -> Script -> VarCtx
allocVars ctx script = inheritVars ctx $ allocScript ctx script

allocScript :: VarCtx -> Script -> VarCtx
allocScript ctx [] = ctx
allocScript ctx (x:xs) = allocScript (allocStmt ctx x) xs

-- TODO: VarAllocator for FunDef
allocStmt :: VarCtx -> Statement -> VarCtx
allocStmt ctx (VarDecl var _)     = allocVar ctx var
allocStmt ctx (ForLoop i _ body)  = allocVar (childScopes ctx [body]) i
allocStmt ctx (WhileLoop _ body)  = childScopes ctx [body]
allocStmt ctx (Condition _ a b)   = childScopes ctx $ a : maybe [] pure b
allocStmt ctx _ = ctx

-- creates new scopes at the next depth
childScopes :: VarCtx -> [Script] -> VarCtx
childScopes (scope, _, (depth, _), table) xs = 
    peerScopes (scope + 1, scope, (depth + 1, 0), table) xs

-- creates new scopes at the same depth
peerScopes :: VarCtx -> [Script] -> VarCtx
peerScopes ctx [] = ctx
peerScopes ctx [script] = allocVars ctx script
peerScopes ctx@(_, prevScope, varPos, _) (script:scripts) = 
    peerScopes (newScope + 1, prevScope, varPos, newTable) scripts
    where (newScope, _, _, newTable) = allocVars ctx script

-- inherits missing variable positions from the previous scope 
inheritVars :: VarCtx -> VarCtx -> VarCtx
inheritVars (scope, prevScope, _, _) (a, b, c, table) = (a, b, c, newTable)
    where 
        vars     = findRow scope table
        prevVars = findRow prevScope table
        newVars  = filterCells prevVars vars
        newTable = updateRow scope newVars table

-- adds a variable position to the current scope
allocVar :: VarCtx -> VarDef -> VarCtx
allocVar ctx (name, dataType) = newCtx
    where 
        (scope, prevScope, pos@(depth, offset), table) = ctx
        newTable = insertCell scope name (pos, 1) table
        newCtx = (scope, prevScope, (depth, offset + 1), newTable)

-----------------------------------------------------------------------------
-- type checking
-----------------------------------------------------------------------------

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | EmptyDecl     SourcePos String    -- variable decl. with no type and value
    | NoReturn      SourcePos           -- function decl. without return