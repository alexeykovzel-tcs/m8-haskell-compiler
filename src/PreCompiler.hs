{-# LANGUAGE FlexibleInstances #-}

module PreCompiler (initCtx, precompile) where

import Parser
import PostParser
import SprockellExt
import Debug.Trace
import Control.Monad (join)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

initCtx :: Script -> Context
initCtx prog = Ctx 0 1 (funs ctx) (scopes ctx) hardPath userRegs
    where 
        ctx = findScopes prog
        -- hardPath = Map.fromList [(1, 2), (2, 3)]
        hardPath = path ctx

-----------------------------------------------------------------------------
-- scope information
-----------------------------------------------------------------------------

type VarTable = [(VarName, (VarPos, VarSize))]

data ScopeCtx = ScopeCtx {
    currId  :: ScopeID,
    prevId  :: ScopeID,
    depth   :: Depth,
    scopes  :: ScopeMap,
    path    :: ScopePath,
    funs    :: FunMap
}

-- collects information about program scopes
findScopes :: Script -> ScopeCtx
findScopes prog = allocScript prog initCtx
    where
        mainScope = (0, (Map.empty, 0, 0))
        initCtx = ScopeCtx {
            currId  = 0, 
            prevId  = 0, 
            depth   = 1, 
            scopes  = Map.fromList [mainScope],
            path    = Map.empty,
            funs    = Map.empty
        }

-- allocates script variables
allocScript :: Script -> ScopeCtx -> ScopeCtx
allocScript script ctx = 
    allocSubScopes script $ ctx { 
        prevId  = currId ctx,
        depth   = depth ctx + 1, 
        scopes  = newScopes
    } where 
        vars              = allocVars (addArp script) (depth ctx, 0)
        (prevVars, _, _)  = fromJust $ Map.lookup (prevId ctx) $ scopes ctx
        fullVars          = Map.union (Map.fromList vars) prevVars
        scopeEntry        = (fullVars, depth ctx, scopeSize vars)
        newScopes         = Map.insert (currId ctx) scopeEntry $ scopes ctx

-- allocates variables of script statements
allocSubScopes :: Script -> ScopeCtx -> ScopeCtx
allocSubScopes [] ctx = ctx
allocSubScopes (x:xs) ctx = 
    allocSubScopes xs $ ctx {
        currId   = currId newCtx,
        scopes   = scopes newCtx,
        path     = newPath,
        funs     = funs newCtx
    } where
        newCtx   = allocStmt x ctx
        newPath  = updatePath ctx newCtx        -- <---- IMPLEMENT THIS

-- updatePath :: Statement -> ScopeCtx -> ScopeCtx -> ScopePath
-- updatePath stmt oldCtx ctx = case stmt of 
--     InScope _               -> Map.insert path
--     WhileLoop _ _           ->  
--     Condition _ _ Nothing   ->
--     Condition _ _ (Just _)  ->
--     FunDef _ _ _ _          ->  
--     _                       -> path

-- allocates statement variables
allocStmt :: Statement -> ScopeCtx -> ScopeCtx 
allocStmt stmt ctx = case stmt of
    
    InScope script      -> allocScript script nextCtx
    WhileLoop _ script  -> allocScript script nextCtx

    Condition _ ifScript Nothing -> allocScript ifScript nextCtx
    Condition _ ifScript (Just elseScript) -> 
        allocScript elseScript $ ctx {
            currId  = elseId,
            scopes  = scopes ifCtx,
            path    = elsePath,
            funs    = funs ifCtx
        } where
            ifCtx       = allocScript ifScript nextCtx
            elseId      = currId ifCtx + 1
            elsePath    = Map.insert nextId elseId $ path ifCtx

    FunDef name args returnType script -> allocScript actRecord funCtx
        where 
            actRecord  = buildAR script args returnType
            funCtx     = updateFuns nextCtx name (nextId, depth ctx - 1)

    _ -> ctx

    where
        nextId  = currId ctx + 1 
        nextCtx = ctx { currId = nextId }

updateFuns :: ScopeCtx -> FunName -> (ScopeID, Depth) -> ScopeCtx
updateFuns ctx name entry = ctx { funs = Map.insert name entry $ funs ctx }

-- calculates scope size based on its variable allocation
scopeSize :: VarTable -> Integer
scopeSize [] = 0
scopeSize ((_, (_, size)):xs) = size + scopeSize xs

-- builds a function activation record
buildAR :: Script -> [VarDef] -> Maybe DataType -> Script
buildAR script args returnType
    = intVar "_rtn"
    : intVar "_sl"
    : script

-- adds ARP variable at the end of the script
addArp :: Script -> Script
addArp script = script ++ [intVar "_arp"]

-- creates an integer variable
intVar :: VarName -> Statement
intVar name = VarDecl (name, IntType) Nothing

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

precompile :: FilePath -> IO()
precompile file = do
    scopeList <- Map.toList <$> scopes <$> scopeCtx
    scopePath <- path <$> scopeCtx 
    printScopes scopeList
    putStrLn $ show scopePath ++ "\n"
    where
        prog      = postScript <$> tryParse script <$> readFile file
        scopeCtx  = findScopes <$> prog

printScopes :: [(ScopeID, (VarMap, Depth, Size))] -> IO()
printScopes scopes = putStrLn $ "\n" ++ scopeTable scopes

scopeTable :: [(ScopeID, (VarMap, Depth, Size))] -> String
scopeTable [] = ""
scopeTable (x:xs) = let (id, (vars, depth, size)) = x
    in show (id, depth, size) ++ " | " 
        ++ scopeRow (Map.toList vars) 
        ++ "\n" ++ scopeTable xs

scopeRow :: [(VarName, (VarPos, VarSize))] -> String
scopeRow [] = ""
scopeRow (x:xs) = let (name, (pos, _)) = x 
    in unwords [show name, show pos, scopeRow xs]