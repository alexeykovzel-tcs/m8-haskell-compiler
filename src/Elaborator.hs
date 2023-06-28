{-# LANGUAGE FlexibleInstances #-}

module Elaborator (
    Depth, Offset,
    VarTable, VarPos,
    ScopeID
) where

import Parser
import Data.Maybe
import Data.Either
import Text.Parsec.Pos
import Control.Monad (join)
import Table

-----------------------------------------------------------------------------
-- position variables in memory
-----------------------------------------------------------------------------

-- builds a table with memory positions for variables by scope
-- (each position contains scope depth and offset from ARP)
varTable :: Script -> VarTable
varTable script = result
    where (_, _, _, result) = allocVars (0, 0, (1, 0), []) script

testVarTable :: FilePath -> IO()
testVarTable file = join $ 
    printTable . varTable . tryParse script <$> readFile file

-----------------------------------------------------------------------------

type ScopeID  = Int
type Offset   = Int
type Depth    = Int

type VarPos   = (Depth, Offset)
type VarTable = Table ScopeID VarName VarPos
type VarCtx   = (ScopeID, ScopeID, VarPos, VarTable)

class VarAllocator a where
    allocVars :: VarCtx -> a -> VarCtx

instance VarAllocator Script where
    allocVars ctx [] = ctx
    allocVars ctx (x:xs) = allocVars (allocVars ctx x) xs

-- TODO: VarAllocator for FunDef
instance VarAllocator Statement where
    allocVars ctx (VarDecl (name, _) _)   = allocVar ctx name
    allocVars ctx (ForLoop (i, _) _ body) = allocVar (childScopes ctx [body]) i
    allocVars ctx (WhileLoop _ body)      = childScopes ctx [body]
    allocVars ctx (Condition _ a b)       = childScopes ctx $ a : maybe [] pure b
    allocVars ctx _ = ctx

-- creates new scopes at the next depth
childScopes :: VarCtx -> [Script] -> VarCtx
childScopes (scope, _, (depth, _), table) xs
    = peerScopes (scope + 1, scope, (depth + 1, 0), table) xs

-- creates new scopes at the same depth
peerScopes :: VarCtx -> [Script] -> VarCtx
peerScopes ctx [] = ctx
peerScopes ctx [script] = inheritVars ctx $ allocVars ctx script
peerScopes ctx@(scope, prevScope, pos, _) (script:scripts) 
    = peerScopes (newScope + 1, prevScope, pos, newTable) scripts
    where (newScope, _, _, newTable) = inheritVars ctx $ allocVars ctx script

-- inherits missing variable positions from the previous scope 
inheritVars :: VarCtx -> VarCtx -> VarCtx
inheritVars (scope, prevScope, _, _) (a, b, c, table) = (a, b, c, newTable)
    where 
        vars     = findRow scope table
        prevVars = findRow prevScope table
        newVars  = filterCells prevVars vars
        newTable = updateRow scope newVars table

-- adds a variable position to the current scope
allocVar :: VarCtx -> String -> VarCtx
allocVar (scope, prevScope, pos@(depth, offset), table) name = newCtx
    where 
        newTable = insertCell scope name pos table
        newCtx   = (scope, prevScope, (depth, offset + 1), newTable)

-----------------------------------------------------------------------------
-- type checking
-----------------------------------------------------------------------------

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | EmptyDecl     SourcePos String    -- variable decl. with no type and value
    | NoReturn      SourcePos           -- function decl. without return