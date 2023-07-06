{-# LANGUAGE FlexibleInstances #-}

module PreCompiler (initCtx, postScript, countWorkers) where

import Parser
import SprockellExt
import Debug.Trace
import Control.Monad (join)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- initial context for compilation
initCtx :: Script -> Int -> Context
initCtx prog num = let scopeCtx = studyScopes prog
    in Ctx {
        scopeId      = 0,
        peerId       = 1,
        funMap       = funs scopeCtx,
        glVars       = findGlVars num prog,
        scopeMap     = scopes scopeCtx,
        scopePath    = path scopeCtx,
        freeRegs     = userRegs,
        freeWorkers  = [1..num]
    }

-----------------------------------------------------------------------------
-- counter for workers 
-----------------------------------------------------------------------------

class WorkerCounter a where
    countWorkers :: a -> Integer

instance WorkerCounter Script where
    countWorkers [] = 0
    countWorkers (x:xs) 
        = max (countWorkers x) (countWorkers xs)

instance WorkerCounter Statement where
    countWorkers (Parallel num script) 
        = num * (max 1 $ countWorkers script)

    countWorkers _ = 0

-----------------------------------------------------------------------------
-- post parser
-----------------------------------------------------------------------------

postScript :: Script -> Script
postScript [] = []
postScript (x:xs) = postStmt x ++ postScript xs

postStmt :: Statement -> Script
postStmt (WhileLoop cond script)  = [WhileLoop cond $ postScript script]
postStmt (Parallel num script)    = [Parallel num $ postScript script] 
postStmt (InScope script)         = [InScope $ postScript script]

postStmt (Condition cond ifScript elseScript) = 
    [Condition cond (postScript ifScript) postElse]
    where 
        postElse = case elseScript of
            Just script  -> Just $ postScript script
            Nothing      -> Nothing

-- add variable for code address to the function
postStmt (FunDef name args returnType script) = [
        VarDecl ("_f_" ++ name, IntType) Nothing,
        FunDef name args returnType (postScript script) 
    ]

-- change "for" loop to "while" as it's easier to compile
postStmt (ForLoop i@(name, _) (IterRange from to) script) = [
    InScope [
        VarDecl i (Just $ from),
        VarDecl ("_to", IntType) (Just $ to),
        WhileLoop whileCond whileScript 
    ]]
    where 
        incrI        = VarAssign name $ Add (Var name) (Fixed $ Int 1)
        whileCond    = LessEq (Var name) (Var "_to")
        whileScript  = (postScript script ++ [incrI]) 

postStmt stmt = [stmt]

-----------------------------------------------------------------------------
-- global variables
-----------------------------------------------------------------------------

type GlVarList  = [(VarName, (MemAddr, VarSize))]
type MemAddr    = Int

findGlVars :: MemAddr -> Script -> GlVarMap
findGlVars addr script = Map.fromList $ scriptGlVars addr script

scriptGlVars :: MemAddr -> Script -> GlVarList
scriptGlVars addr [] = []
scriptGlVars addr (x:xs) = 
    let (nextAddr, vars) = stmtGlVars addr x
    in  vars ++ scriptGlVars nextAddr xs

stmtGlVars :: MemAddr -> Statement -> (MemAddr, GlVarList)
stmtGlVars addr stmt = case stmt of
    GlVarDecl (name, dataType) _ -> 
        (addr + 1, [(name, (addr, measureVar dataType))])
    
    _ -> (addr, [])

testGlVars :: FilePath -> IO()
testGlVars file = do
    code <- readFile file
    putStrLn $ show $ findGlVars 5 $ parseWith script code

-----------------------------------------------------------------------------
-- stack scopes
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
studyScopes :: Script -> ScopeCtx
studyScopes prog = allocScript prog initCtx
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
        id       = currId ctx
        newId    = currId newCtx
        newCtx   = allocStmt x ctx
        newPath
            | id == newId || length xs == 0 = path newCtx
            | otherwise = Map.insert (id + 1) (newId + 1) $ path newCtx

-- allocates statement variables
allocStmt :: Statement -> ScopeCtx -> ScopeCtx 
allocStmt stmt ctx = case stmt of
    
    InScope script      -> allocScript script nextCtx
    Parallel _ script   -> allocScript script nextCtx
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
            actRecord   = buildAR script args returnType
            argNames    = fst <$> args
            funCtx      = updateFuns nextCtx name (nextId, depth ctx - 1, argNames)

    _ -> ctx

    where
        nextId  = currId ctx + 1 
        nextCtx = ctx { currId = nextId }

updateFuns :: ScopeCtx -> FunName -> (ScopeID, Depth, [VarName]) -> ScopeCtx
updateFuns ctx name entry = ctx { funs = Map.insert name entry $ funs ctx }

-- calculates scope size based on its variable allocation
scopeSize :: VarTable -> Integer
scopeSize [] = 0
scopeSize ((_, (_, size)):xs) = size + scopeSize xs

-- builds a function activation record
buildAR :: Script -> [VarDef] -> Maybe DataType -> Script
buildAR script args returnType
    = intVar "_rtn_val"
    : intVar "_rtn_addr"
    : intVar "_link"
    : declVars args
    ++ script

-- declares variables based on definitions
declVars :: [VarDef] -> Script
declVars [] = []
declVars (x:xs) = VarDecl x Nothing : declVars xs

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
