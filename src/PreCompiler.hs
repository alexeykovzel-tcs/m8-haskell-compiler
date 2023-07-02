{-# LANGUAGE FlexibleInstances #-}

module PreCompiler (initCtx) where

import Parser
import PostParser
import Data.Maybe
import SprockellExt
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join)

-- builds the initial context for compilation
initCtx :: Script -> Context
initCtx prog = Ctx 0 (allocVars prog) (walkScopes prog) userRegs

-- :::: FOR TESTING ::::

printScopeMap :: FilePath -> IO()
printScopeMap file = join $ printTable <$> (Map.toList <$> (allocVars <$> prog))
    where prog = tryParse script <$> readFile file

printTable [] = pure ()
printTable ((k,(v, d, s)):xs) = do
    putStrLn $ show (k, d, s) ++ " " ++ printRow (Map.toList v)
    printTable xs

printRow (x:xs) = show x ++ " " ++ printRow xs
printRow [] = ""

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

type VarCtx    = (ScopeID, ScopeID, VarPos, ScopeMap)

-- builds a table with variable positions in memory
allocVars :: Script -> ScopeMap
allocVars script = scopeMap
    where 
        initScopeMap        = Map.fromList [(0, (Map.empty, 1, 0))]
        (_, _, _, scopeMap) = finishScope $ scriptVars (-1, 0, (1, 0), initScopeMap) script

-- allocates variables for a given script
scriptVars :: VarCtx -> Script -> VarCtx
scriptVars ctx []     = ctx
scriptVars ctx (x:xs) = scriptVars nextCtx xs
    where 
        ctx2 = incrScope $ incrDepth $ finishScope ctx
        nextCtx = case x of
            VarDecl var _      -> allocVar ctx var
            InScope body       -> scriptVars ctx2 body 
            WhileLoop _ body   -> scriptVars ctx2 body
            Condition _ a b    -> peerVars ctx2 $ a : maybe [] pure b
            _                  -> ctx

-- allocates variables for scripts on the same depth
peerVars :: VarCtx -> [Script] -> VarCtx
peerVars ctx [x] = scriptVars ctx x
peerVars ctx@(_, _, pos, _) (x:xs) =
    let (parent, scope, _, scopeMap) = incrScope $ finishScope $ scriptVars ctx x
    in  peerVars (parent, scope, pos, scopeMap) xs

-- allocates a variable in the current scope
allocVar :: VarCtx -> VarDef -> VarCtx
allocVar (parent, scope, pos@(depth, offset), scopeMap) (name, dataType) =
         (parent, scope, (depth, offset + varSize), newScopeMap)
    where 
        (varMap, depth, scopeSize) = fromJust $ Map.lookup scope scopeMap
        varSize      = measureVar dataType
        newVarMap    = Map.insert name (pos, varSize) varMap
        newScopeMap  = Map.insert scope (newVarMap, depth, scopeSize + varSize) scopeMap

-- adds a new scope
incrScope :: VarCtx -> VarCtx
incrScope (parent, scope, pos@(depth, _), scopeMap) = newCtx
    where 
        newScopeMap = Map.insert (scope + 1) (Map.empty, depth, 0) scopeMap
        newCtx = (parent, scope + 1, pos, newScopeMap)

finishScope :: VarCtx -> VarCtx
finishScope ctx = inheritVars $ allocVar ctx ("_arp", IntType)

-- inherits missing variables from the previous scope
inheritVars :: VarCtx -> VarCtx
inheritVars ctx@(_, _, (1, _), _) = ctx
inheritVars (parent, scope, pos@(depth, _), scopeMap) = 
            (parent, scope, pos, newScopeMap)
    where
        (parentVarMap, _, _)    = fromJust $ Map.lookup parent scopeMap
        (varMap, depth, size)   = fromJust $ Map.lookup scope scopeMap
        newVarMap               = Map.union varMap parentVarMap
        newScopeMap             = Map.insert scope (newVarMap, depth, size) scopeMap

-- increases scope depth
incrDepth :: VarCtx -> VarCtx
incrDepth (_, scope, (depth, _), scopeMap) =
    (scope, scope, (depth + 1, 0), scopeMap)

-- measures the variable size based on its type
measureVar :: DataType -> VarSize
measureVar (ArrType _ size) = size
measureVar _                = 1