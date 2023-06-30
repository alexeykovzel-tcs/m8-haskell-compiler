{-# LANGUAGE FlexibleInstances #-}

module Scopes (scopeCtx) where

import Parser
import Common.Table

type ScopeID   = Integer
type ScopeCtx  = (VarTable, ScopePath)

scopeCtx :: Script -> ScopeCtx
scopeCtx script = (varTable, scopePath)
    where 
        scopePath  = walkScopes script
        varTable   = inheritVars (allocVars script) scopePath

testScopeCtx :: FilePath -> IO()
testScopeCtx file = do
    (varTable, scopePath) <- scopeCtx . tryParse script <$> readFile file
    putStrLn ""
    printTable varTable
    putStrLn "\nscope path:"
    putStrLn $ show scopePath
    putStrLn ""

-----------------------------------------------------------------------------
-- scope hierarchy
-----------------------------------------------------------------------------

type ScopePath = [(ScopeID, ScopeID)]

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
            ForLoop _ _ body -> scopes nextCtx body
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

type Offset      = Integer
type Depth       = Integer

type VarSize     = Integer
type VarPos      = (Depth, Offset)
type VarTable    = Table ScopeID VarName (VarPos, VarSize)
type VarCtx      = (ScopeID, VarPos, VarTable)

-- builds a tables with variable positions in memory
allocVars :: Script -> VarTable
allocVars script = varTable
    where (_, _, varTable) = scriptVars (0, (1, 0), []) script

-- allocates variables for a given script
scriptVars :: VarCtx -> Script -> VarCtx
scriptVars ctx []     = ctx
scriptVars ctx (x:xs) = scriptVars nextCtx xs
    where 
        ctx2 = nextScope ctx
        nextCtx = case x of
            VarDecl var _      -> allocVar ctx var
            ForLoop i _ body   -> scriptVars (allocVar ctx2 i) body
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