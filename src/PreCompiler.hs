{-# LANGUAGE FlexibleInstances #-}

module PreCompiler (initCtx) where

import Parser
import PostParser
import Common.Table
import Common.SprockellExt
import qualified Data.Map as Map
import Control.Monad (join)

type ScopeCtx  = (VarTable, ScopePath)

-- builds the initial context for compilation
initCtx :: Script -> Context
initCtx prog = Ctx mainScope scopeMap scopePath userRegs
    where
        scopePath  = walkScopes prog
        varTable   = allocVars prog scopePath
        scopeMap   = toScopeMap (Map.fromList scopePath) (tableToMap varTable)
        mainScope  = (0, 1)

-- prints a table with variable positions after parsing a file
printVarTable :: FilePath -> IO()
printVarTable file = join $ printTable <$> (allocVars <$> prog <*> scopePath)
    where 
        prog = postParse <$> (tryParse script <$> readFile file) 
        scopePath = walkScopes <$> prog

-----------------------------------------------------------------------------
-- scope hierarchy
-----------------------------------------------------------------------------

-- stores data about child-parent relationships between scopes 
walkScopes :: Script -> ScopePath
walkScopes script = reverse path
    where (path, _) = scopes ([], 0) script 

class ScopeWalker a where
    scopes :: (ScopePath, ScopeID) -> a -> (ScopePath, ScopeID)

instance ScopeWalker Script where
    scopes = foldl scopes

instance ScopeWalker Statement where
    scopes ctx@(path, last) stmt = 
        let nextCtx = ((last + 1, last) : path, last + 1) 
        in case stmt of
            InScope     body -> scopes nextCtx body 
            WhileLoop _ body -> scopes nextCtx body
            Condition _ ifBody elseBody -> 
                let 
                    ifCtx@(ifScopes, ifLast) = scopes nextCtx ifBody
                    elseCtx = ((ifLast + 1, last) : ifScopes, ifLast + 1)
                in 
                    maybe ifCtx (scopes elseCtx) elseBody
            _ -> ctx

-----------------------------------------------------------------------------
-- variable positions in memory
-----------------------------------------------------------------------------

type VarTable    = Table ScopeID VarName (VarPos, VarSize)
type VarCtx      = (ScopeID, VarPos, VarTable)

-- builds a table with variable positions in memory
allocVars :: Script -> ScopePath -> VarTable
allocVars script scopePath = inheritVars varTable scopePath
    where (_, _, varTable) = scriptVars (0, (1, 0), []) script

-- allocates variables for a given script
scriptVars :: VarCtx -> Script -> VarCtx
scriptVars ctx []     = ctx
scriptVars ctx (x:xs) = scriptVars nextCtx xs
    where 
        ctx2 = nextScope ctx
        nextCtx = case x of
            VarDecl var _      -> allocVar ctx var
            InScope body       -> scriptVars ctx2 body 
            WhileLoop _ body   -> scriptVars ctx2 body
            Condition _ a b    -> peerVars ctx2 $ a : maybe [] pure b
            _                  -> ctx

-- increases depth and scope id
nextScope :: VarCtx -> VarCtx
nextScope (scope, (depth, _), table) = 
    (scope + 1, (depth + 1, 0), table)

-- allocates variables for scripts on the same depth
peerVars :: VarCtx -> [Script] -> VarCtx
peerVars ctx [] = ctx
peerVars ctx [x] = scriptVars ctx x
peerVars ctx@(_, pos, _) (x:xs) = 
    let (scope, _, table) = scriptVars ctx x
    in peerVars (scope + 1, pos, table) xs

-- allocates a variable in the current scope
allocVar :: VarCtx -> VarDef -> VarCtx
allocVar (scope, pos@(depth, offset), table) (name, _) =
    let newTable = insertCell scope name (pos, 1) table
    in (scope, (depth, offset + 1), newTable)

-- inherits missing variables from the previous scopes
inheritVars :: VarTable -> ScopePath -> VarTable
inheritVars table [] = table
inheritVars table ((scope, prevScope):xs) = inheritVars newTable xs
    where 
        vars      = findRow scope table
        prevVars  = findRow prevScope table
        newVars   = filterCells prevVars vars
        newTable  = updateRow scope newVars table