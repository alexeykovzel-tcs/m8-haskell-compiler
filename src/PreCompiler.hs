{-# LANGUAGE FlexibleInstances #-}

module PreCompiler where

import Parser
import SprockellExt
import Debug.Trace
import Control.Monad (join)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

initCtx :: Script -> Context
initCtx prog = Ctx 0 funMap (scopes scopeCtx) (path scopeCtx) userRegs
    where
        scopeCtx   = findScopes prog
        funMap     = Map.empty

-----------------------------------------------------------------------------
-- scope information
-----------------------------------------------------------------------------

type VarTable = [(VarName, (VarPos, VarSize))]

data ScopeCtx = ScopeCtx {
    id      :: ScopeID,
    prevId  :: ScopeID,
    depth   :: Depth,
    scopes  :: ScopeMap,
    path    :: ScopePath
}

-- collects information about program scopes
findScopes :: Script -> ScopeCtx
findScopes prog = 
    let initCtx = ScopeCtx 0 0 1 (Map.fromList [(0, (Map.empty, 0, 0))]) Map.empty
    in  allocScript initCtx prog

-- allocates script variables
allocScript :: ScopeCtx -> Script -> ScopeCtx
allocScript (ScopeCtx id prevId depth scopes path) script 
    = allocSubScopes (ScopeCtx id id (depth + 1) newScopes path) script
    where 
        vars              = allocVars (addArp script) (depth, 0)
        (prevVars, _, _)  = fromJust $ Map.lookup prevId scopes
        fullVars          = Map.union (Map.fromList vars) prevVars
        newScopes         = Map.insert id (fullVars, depth, scopeSize vars) scopes

-- allocates variables of script statements
allocSubScopes :: ScopeCtx -> Script -> ScopeCtx
allocSubScopes ctx [] = ctx
allocSubScopes ctx@(ScopeCtx _ prevId depth _ _) (x:xs) 
    = allocSubScopes newCtx xs
    where
        ScopeCtx id _ _ scopeMap path = allocStmt ctx x
        newCtx = ScopeCtx id prevId depth scopeMap path

-- allocates statement variables
allocStmt :: ScopeCtx -> Statement -> ScopeCtx 
allocStmt ctx@(ScopeCtx id prevId depth scopes path) stmt = 
    let nextCtx = ScopeCtx (id + 1) prevId depth scopes path
    in case stmt of
    
    InScope script      -> allocScript nextCtx script
    WhileLoop _ script  -> allocScript nextCtx script

    Condition _ ifScript Nothing           -> allocScript nextCtx ifScript
    Condition _ ifScript (Just elseScript) -> allocScript elseCtx elseScript
        where
            ScopeCtx ifId _ _ ifScopes ifPath = allocScript nextCtx ifScript
            elseCtx = ScopeCtx (ifId + 1) prevId depth ifScopes elsePath
            elsePath = Map.insert id (ifId + 1) ifPath

    -- FunDef name args returnType script -> 

    _ -> ctx

-- calculates scope size based on its variable allocation
scopeSize :: VarTable -> Integer
scopeSize [] = 0
scopeSize ((_, (_, size)):xs) = size + scopeSize xs

-- adds ARP variable at the end of the script
addArp :: Script -> Script
addArp script = script ++ [VarDecl ("_arp", IntType) Nothing]

-- allocates "direct" script variables  
allocVars :: Script -> VarPos -> VarTable
allocVars [] _ = []
allocVars (x:xs) varPos@(depth, offset) = case x of

    VarDecl (name, dataType) _
        -> varEntry : allocVars xs (depth, offset + varSize)
        where
            varSize = measureVar dataType
            varEntry = (name, (varPos, varSize))
        
    _ -> allocVars xs varPos
    
-- calculates variable size based on its data type
measureVar :: DataType -> VarSize
measureVar (ArrType _ size) = size
measureVar _ = 1

-----------------------------------------------------------------------------
-- testing
-----------------------------------------------------------------------------

testFile :: FilePath -> IO()
testFile file = join $ printScopes <$> Map.toList <$> scopeMap 
    where
        prog      = tryParse script <$> readFile file
        scopeMap  = scopes <$> findScopes <$> prog

printScopes :: [(ScopeID, (VarMap, Depth, Size))] -> IO()
printScopes [] = pure ()
printScopes ((id, (vars, depth, size)):xs) = do
    putStrLn $ show (id, depth, size) ++ " " 
        ++ scopeRow (Map.toList vars)
    printScopes xs

scopeRow :: [(VarName, (VarPos, VarSize))] -> String
scopeRow (x:xs) = show x ++ " " ++ scopeRow xs
scopeRow [] = ""