{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile) where

import Sprockell
import Elaborator
import Data.Maybe
import Data.Char
import Table (tableToMap)
import Debug.Trace (trace)
import Parser (VarName, FunName)
import qualified Parser as AST
import qualified Data.Map as Map

type Scope      = (ScopeID, Depth)
type ScopeSize  = Integer
type ScopeInfo  = (VarMap, ScopeSize, ScopeID)
type ScopeMap   = Map.Map ScopeID ScopeInfo
type VarMap     = Map.Map VarName (VarPos, VarSize)

data Context = Ctx {
    lastScope :: Scope,
    scopeMap  :: ScopeMap,
    freeRegs  :: [RegAddr]
}

compile :: String -> [Instruction]
compile code = initDP ++ progASM ++ [EndProg]
    where
        prog     = AST.tryParse AST.script code
        progCtx  = initCtx prog
        progASM  = compileScript progCtx prog

initDP :: [Instruction]
initDP = [Load (ImmValue 0) regDP]

initCtx :: AST.Script -> Context
initCtx prog = Ctx (0, 1) scopeMap userRegs
    where
        (varTable, path) = scopeCtx prog
        scopeMap  = toScopeMap pathMap varMap
        pathMap   = Map.fromList path
        varMap    = tableToMap varTable

toScopeMap :: Map.Map ScopeID ScopeID -> Map.Map ScopeID VarMap -> ScopeMap
toScopeMap pathMap varMap = Map.mapWithKey getInfo varMap
    where 
        getInfo id varMap  = (varMap, scopeSize varMap, prevScope id)
        scopeSize varMap   = sumSeconds $ Map.elems varMap
        prevScope id       = fromMaybe (-1) (Map.lookup id pathMap)

-----------------------------------------------------------------------------
-- script compilation
-----------------------------------------------------------------------------

compileScript :: Context -> AST.Script -> [Instruction]
compileScript ctx [] = []
compileScript ctx (x:xs) = (compileStmt ctx x) ++ (compileScript ctx xs)

compileStmt :: Context -> AST.Statement -> [Instruction]
compileStmt ctx stmt = case stmt of

    AST.VarDecl (name, _) Nothing      -> []
    AST.VarDecl (name, _) (Just expr)  -> compileVarExpr ctx name expr
    AST.VarAssign name expr            -> compileVarExpr ctx name expr
    AST.Action expr                    -> compileExpr ctx2 expr reg2
    
    AST.WhileLoop expr script 
        -> cond ++ branch ++ body ++ jumpBack
        where
            cond      = compileCond ctx2 expr reg2 
            branch    = [Branch reg2 $ Rel relSkip]
            relSkip   = (length body) + 2
            body      = inScope ctx $ compileScript (inScopeCtx ctx) script
            jumpBack  = [Jump $ Rel relBack]
            relBack   = - (length body) - (length branch) - (length cond)
    
    -- TODO: Update ARP
    AST.ForLoop (iter, _) (AST.IterRange from to) script 
        -> inScope ctx $ saveIter ++ loadIter ++ branch 
                          ++ body ++ incrIter ++ jumpBack
        where
            ctxScp    = inScopeCtx ctx
            saveIter  = updateVarImm ctxScp iter from
            loadIter  = loadVar ctxScp2 iter reg2
            branch    = [loadImm to reg3,             -- load 'to'
                         Compute Gt reg2 reg3 reg3,   -- check if 'i' > 'to'
                         Branch reg3 $ Rel relSkip]   -- skip if true
            body      = compileScript ctxScp script
            incrIter  = incrMem ctxScp iter
            jumpBack  = [Jump $ Rel relBack]
            reg3      = findReg ctxScp2
            relSkip   = (length body) + (length incrIter) + 2
            relBack   = - (length body) - (length incrIter) 
                        - (length loadIter) - (length branch)
            (reg2, ctxScp2) = occupyReg ctxScp

    -- compileScript does not store last scope... 
    AST.Condition expr ifScript elseScript 
        -> cond ++ branch ++ ifBody ++ elseBody
        where
            ctxScp    = inScopeCtx ctx
            ctxScp2   = ... -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            cond      = compileCond ctx2 expr reg2
            branch    = [Branch reg2 $ Rel $ length ifBody + 1]
            ifBody    = inScope ctx (compileScript ctxScp ifScript) ++ skipElse
            elseBody  = maybe [] (inScope ctx . compileScript ctxScp2) elseScript
            skipElse  = maybe [] (\_ -> jumpElse) elseScript
            jumpElse  = [Jump $ Rel $ length elseBody + 1]

    where (reg2, ctx2) = occupyReg ctx

-- offset DP by the size of the current scope 
inScopeCtx :: Context -> Context
inScopeCtx (Ctx (scope, scopeDepth) scopeMap regs) = 
    Ctx (scope + 1, scopeDepth + 1) scopeMap regs

inScope :: Context -> [Instruction] -> [Instruction]
inScope ctx body = (moveScope ctx 1) ++ body ++ (moveScope ctx $ -1)

moveScope :: Context -> Integer -> [Instruction]
moveScope ctx@(Ctx (scope, _) scopeMap _) mult = 
    [loadImm (toInteger mult * offsetDP) reg, Compute Add reg regDP regDP]
    where 
        (_, offsetDP, _) = fromJust $ Map.lookup scope scopeMap
        reg = findReg ctx

compileCond :: Context -> AST.Expr -> RegAddr -> [Instruction]
compileCond ctx expr reg = 
    (compileExpr ctx expr reg) ++ (notBool ctx reg)

compileVarExpr :: Context -> VarName -> AST.Expr -> [Instruction]
compileVarExpr ctx name expr = exprToReg ++ varToMem
    where 
        (reg2, ctx2) = occupyReg ctx
        exprToReg    = compileExpr ctx2 expr reg2
        varToMem     = updateVar ctx2 name reg2

-----------------------------------------------------------------------------
-- expression compilation
-----------------------------------------------------------------------------

compileExpr :: Context -> AST.Expr -> RegAddr -> [Instruction]
compileExpr ctx expr reg = case expr of

    -- binary operations
    AST.Mult    e1 e2 -> compileBin ctx e1 e2 reg Mul
    AST.Add     e1 e2 -> compileBin ctx e1 e2 reg Add
    AST.Sub     e1 e2 -> compileBin ctx e1 e2 reg Sub
    AST.Eq      e1 e2 -> compileBin ctx e1 e2 reg Equal
    AST.MoreEq  e1 e2 -> compileBin ctx e1 e2 reg GtE
    AST.LessEq  e1 e2 -> compileBin ctx e1 e2 reg LtE
    AST.More    e1 e2 -> compileBin ctx e1 e2 reg Gt
    AST.Less    e1 e2 -> compileBin ctx e1 e2 reg Lt
    AST.Both    e1 e2 -> compileBin ctx e1 e2 reg And
    AST.OneOf   e1 e2 -> compileBin ctx e1 e2 reg Or

    -- load variable value
    AST.Var name -> loadVar ctx name reg

    -- TODO: Arrays, String and None
    AST.Fixed (AST.Int  val)  -> [loadImm val reg]
    AST.Fixed (AST.Bool val)  -> [Load (ImmValue $ intBool val) reg]
    AST.Fixed (AST.None)      -> [Load (ImmValue $ -1) reg]
    AST.Fixed (AST.Arr vals)  -> []
    AST.Fixed (AST.Text text) -> []

    AST.Ternary expr e1 e2
        -> cond ++ branch ++ ifBody ++ elseBody
        where
            cond      = compileCond ctx expr reg
            branch    = [Branch reg $ Rel $ length ifBody + 1]
            ifBody    = compileExpr ctx e1 reg ++ jumpElse
            elseBody  = compileExpr ctx e2 reg
            jumpElse  = [Jump $ Rel $ length elseBody + 1]

    -- embedded functions
    AST.FunCall "thread_create" ((AST.Fixed (AST.Int threadId)):_)
        -> writeString ctx ("create thread: " ++ show threadId ++ "\n")

    AST.FunCall "thread_join" ((AST.Fixed (AST.Int threadId)):_)
        -> writeString ctx ("join thread: " ++ show threadId ++ "\n")
 
    AST.FunCall "print" ((AST.Fixed (AST.Text msg)):_)
        -> writeString ctx (msg ++ "\n")

    AST.FunCall "print" (expr:_)
        -> compileExpr ctx expr reg 
        ++ [WriteInstr reg numberIO]

-- compiles a binary operation
compileBin :: Context -> AST.Expr -> AST.Expr 
           -> RegAddr -> Operator -> [Instruction]

compileBin ctx e1 e2 reg op = 
    c1 ++ c2 ++ [Compute op reg reg2 reg]
    where 
        (reg2, ctx2) = occupyReg ctx
        c1 = compileExpr ctx  e1 reg
        c2 = compileExpr ctx2 e2 reg2

-----------------------------------------------------------------------------
-- variable instructions
-----------------------------------------------------------------------------

-- loads variable data from memory to a register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = dpToReg ++ valToReg
    where 
        (depth, offset) = locateVar ctx name
        (reg2, ctx2)    = occupyReg ctx
        dpToReg         = loadDP ctx2 depth reg2
        valToReg        = offsetLoad ctx2 reg2 offset reg

-- updates a variable in memory from register
updateVar :: Context -> VarName -> RegAddr -> [Instruction]
updateVar ctx name reg = dpToReg ++ varToMem
    where
        (depth, offset) = locateVar ctx name
        (reg2, ctx2)    = occupyReg ctx
        dpToReg         = loadDP ctx2 depth reg2
        varToMem        = offsetStore ctx2 reg reg2 offset

-- updates a variable in memory by value
updateVarImm :: Context -> VarName -> Integer -> [Instruction]
updateVarImm ctx name val = 
    (loadImm val reg2) : (updateVar ctx2 name reg2)
    where (reg2, ctx2) = occupyReg ctx

-- loads a data pointer at a given depth
loadDP :: Context -> Depth -> RegAddr -> [Instruction]
loadDP ctx depth reg = 
    [loadImm dpOffset reg, Compute Add reg regDP reg]
    where 
        (scope, scopeDepth) = lastScope ctx
        depthDiff = scopeDepth - depth
        dpOffset = - (scopeOffset ctx scope depthDiff)

-----------------------------------------------------------------------------
-- context helpers
-----------------------------------------------------------------------------

scopeOffset :: Context -> ScopeID -> Depth -> Offset
scopeOffset _   _     0     = 0
scopeOffset ctx scope depthDiff = 
    scopeOffset ctx prevScope (depthDiff - 1) + prevScopeSize
    where 
        prevScope     = findPrevScope ctx scope
        prevScopeSize = fromInteger $ measureScope ctx prevScope

findPrevScope :: Context -> ScopeID -> ScopeID
findPrevScope ctx scope = prevScope
    where (_, _, prevScope) = analyzeScope ctx scope 

measureScope :: Context -> ScopeID -> ScopeSize
measureScope ctx scope = scopeSize
    where (_, scopeSize, _) = analyzeScope ctx scope 

analyzeScope :: Context -> ScopeID -> ScopeInfo
analyzeScope ctx scope = fromJust $ Map.lookup scope $ scopeMap ctx

locateVar :: Context -> VarName -> VarPos
locateVar (Ctx (scopeId, _) scopeMap _) name = varPos
    where 
        (varMap, _, _) = fromJust $ Map.lookup scopeId scopeMap
        (varPos, _)    = fromJust $ Map.lookup name varMap

-----------------------------------------------------------------------------
-- register management
-----------------------------------------------------------------------------

-- Reserve register for a data pointer
regDP = regA

-- registers for free usage
userRegs = [regB, regC, regD, regE, regF]

-- occupies a free register
occupyReg :: Context -> (RegAddr, Context)
occupyReg (Ctx s v (r:rs)) = (r, Ctx s v rs)

-- finds a free register
findReg :: Context -> RegAddr
findReg ctx = let (r:_) = freeRegs ctx in r

-- copies value from reg1 to reg2
copyReg :: RegAddr -> RegAddr -> Instruction
copyReg reg1 reg2 = Compute Add reg0 reg1 reg2

-- loads value to reg 
loadImm :: Integer -> RegAddr -> Instruction
loadImm val reg = Load (ImmValue $ fromInteger val) reg

-- reverses boolean in reg
notBool :: Context -> RegAddr -> [Instruction]
notBool ctx reg = [Compute Equal reg reg0 reg]

addImm :: Context -> Integer -> RegAddr -> [Instruction]
addImm ctx val reg = let reg2 = findReg ctx in 
    [loadImm val reg2, Compute Add reg reg2 reg]

-----------------------------------------------------------------------------
-- memory management
-----------------------------------------------------------------------------

-- increments a variable in memory
incrMem :: Context -> VarName -> [Instruction]
incrMem ctx name = addMem ctx name 1

-- adds value to a variable in memory
addMem :: Context -> VarName -> Integer -> [Instruction]
addMem ctx name val = let (reg2, ctx2) = occupyReg ctx in
       loadVar    ctx2 name reg2
    ++ addImm     ctx2 val  reg2 
    ++ updateVar  ctx2 name reg2

-- loads from memory with an offset
offsetLoad :: Context -> RegAddr -> Offset -> RegAddr -> [Instruction]
offsetLoad ctx reg1 offset reg2 = 
    [
        loadImm offset reg2,
        Compute Add reg1 reg2 reg2,
        Load (IndAddr reg2) reg2
    ]

-- stores to memory with an offset
offsetStore :: Context -> RegAddr -> RegAddr -> Offset -> [Instruction]
offsetStore ctx reg1 reg2 offset =
    [
        loadImm offset reg3,
        Compute Add reg3 reg2 reg2,
        Store reg1 (IndAddr reg2)
    ]
    where reg3 = findReg ctx

-----------------------------------------------------------------------------
-- IO instructions
-----------------------------------------------------------------------------

-- code to print a string
writeString :: Context -> String -> [Instruction]
writeString ctx str = concat $ map (writeChar $ findReg ctx) str

-- code to print a single character
writeChar :: RegAddr -> Char -> [Instruction]
writeChar reg c = [Load (ImmValue $ ord c) reg, WriteInstr reg charIO]

-----------------------------------------------------------------------------
-- Other utilities
-----------------------------------------------------------------------------

sumSeconds :: [(a, Integer)] -> Integer
sumSeconds [] = 0
sumSeconds ((_, x):xs) = x + sumSeconds xs 