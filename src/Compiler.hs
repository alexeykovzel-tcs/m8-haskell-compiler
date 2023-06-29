{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile) where

import Sprockell
import Elaborator
import Data.Maybe
import Data.Char
import Table (tableToMap)
import Parser (VarName, FunName)
import qualified Parser as AST
import qualified Data.Map as Map

type Scope      = (ScopeID, Depth)
type ScopeSize  = Integer
type ScopeMap   = Map.Map ScopeID (VarMap, ScopeSize)
type VarMap     = Map.Map VarName (VarPos, VarSize)

data Context = Ctx {
    scope    :: Scope,
    scopeMap :: ScopeMap,
    freeRegs :: [RegAddr]
}

compile :: String -> [Instruction]
compile code = preCompile ++ compileScript ctx prog ++ [EndProg]
    where
        prog = AST.tryParse AST.script code
        ctx = Ctx (0, 1) (inferScopes prog) userRegs

-----------------------------------------------------------------------------
-- compilation preprocessing
-----------------------------------------------------------------------------

-- TODO: Build "main" activation record
preCompile :: [Instruction]
preCompile = [Load (ImmValue 0) regDP]

inferScopes :: AST.Script -> ScopeMap
inferScopes prog = scopeSizes $ tableToMap $ varTable prog 

scopeSizes :: Map.Map ScopeID VarMap -> ScopeMap
scopeSizes scopeMap = Map.map measure scopeMap
    where measure varMap = (varMap, sumSeconds $ Map.elems varMap)

sumSeconds :: [(a, Integer)] -> Integer
sumSeconds [] = 0
sumSeconds ((_, x):xs) = x + sumSeconds xs 

-----------------------------------------------------------------------------
-- script compilation
-----------------------------------------------------------------------------

compileScript :: Context -> AST.Script -> [Instruction]
compileScript ctx [] = []
compileScript ctx (x:xs) = (compileStmt ctx x) ++ (compileScript ctx xs)

compileStmt :: Context -> AST.Statement -> [Instruction]
compileStmt ctx stmt = case stmt of

    AST.VarDecl (name, _) Nothing      -> []
    AST.VarDecl (name, _) (Just expr)  -> compileVar ctx name expr
    AST.VarAssign name expr            -> compileVar ctx name expr
    AST.Action expr                    -> compileExpr ctx2 expr reg2
    
    AST.WhileLoop expr script 
        -> cond ++ branch ++ bodyScope ++ jumpBack
        where
            cond      = compileCond ctx2 expr reg2 
            branch    = [Branch reg2 $ Rel relSkip]
            relSkip   = (length body) + 2
            body      = compileScript (nextScope ctx) script
            bodyScope = (scopeDown ctx) ++ body ++ (scopeUp ctx)
            jumpBack  = [Jump $ Rel relBack]
            relBack   = - (length body) - (length branch) - (length cond)
    
    -- TODO: Update ARP
    AST.ForLoop (iter, _) (AST.IterRange from to) script 
        -> saveIter ++ loadIter ++ branch ++ body ++ incrIter ++ jumpBack
        where 
            saveIter  = updateVarImm ctx iter from
            loadIter  = loadVar ctx2 iter reg2
            branch    = [loadImm to reg3,             -- load 'to'
                         Compute Gt reg2 reg3 reg3,   -- check if 'i' > 'to'
                         Branch reg3 $ Rel relSkip]   -- skip if true
            body      = compileScript ctx script
            incrIter  = incrMem ctx iter
            jumpBack  = [Jump $ Rel relBack]
            reg3      = findReg ctx2
            relSkip   = (length body) + (length incrIter) + 2
            relBack   = - (length body) - (length incrIter) 
                        - (length loadIter) - (length branch)

    AST.Condition expr ifScript elseScript 
        -> cond ++ branch ++ ifBody ++ elseBody
        where
            cond      = compileCond ctx2 expr reg2
            branch    = [Branch reg2 $ Rel $ length ifBody + 1]
            ifBody    = compileScript ctx ifScript ++ skipElse -- TODO: increase depth & scope
            elseBody  = maybe [] (compileScript ctx) elseScript -- TODO: increase scope
            skipElse  = maybe [] (\_ -> jumpElse) elseScript
            jumpElse  = [Jump $ Rel $ length elseBody + 1]

    -- TODO: Implement these:
    -- AST.FunDef name args returnType script -> []
    -- AST.ReturnVal expr -> []
    -- AST.ArrInsert name idx expr -> []

    where (reg2, ctx2) = occupyReg ctx

-- offset DP by the size of the current scope 
nextScope :: Context -> Context
nextScope (Ctx (scopeId, scopeDepth) scopeMap regs) = 
    Ctx (scopeId + 1, scopeDepth + 1) scopeMap regs

scopeDown :: Context -> [Instruction]
scopeDown ctx = moveScope ctx 1

scopeUp :: Context -> [Instruction]
scopeUp ctx = moveScope ctx (-1)

moveScope :: Context -> Int -> [Instruction]
moveScope ctx@(Ctx (scopeId, _) scopeMap _) mult = 
    [loadImm (toInteger mult * offsetDP) reg, Compute Add reg regDP regDP]
    where 
        (_, offsetDP) = fromJust $ Map.lookup scopeId scopeMap
        reg = findReg ctx

compileCond :: Context -> AST.Expr -> RegAddr -> [Instruction]
compileCond ctx expr reg = 
    (compileExpr ctx expr reg) ++ (reverseBool ctx reg)

compileVar :: Context -> VarName -> AST.Expr -> [Instruction]
compileVar ctx name expr = exprToReg ++ varToMem
    where 
        (reg2, ctx2) = occupyReg ctx
        exprToReg = compileExpr ctx2 expr reg2
        varToMem = updateVar ctx2 name reg2

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
    AST.Fixed (AST.Int  val)  -> [Load (ImmValue $ fromInteger val) reg]
    AST.Fixed (AST.Bool val)  -> [Load (ImmValue $ intBool val) reg]
    AST.Fixed (AST.None)      -> [Load (ImmValue (-1)) reg]
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

    -- TODO: Implement this:
    -- AST.FunCall name args -> []
    -- AST.Lambda args script   -> []

-- compiles a binary operation
compileBin :: Context -> AST.Expr -> AST.Expr 
           -> RegAddr -> Operator -> [Instruction]

compileBin ctx e1 e2 reg op = 
    c1 ++ c2 ++ [Compute op reg reg2 reg]
    where 
        (reg2, ctx2) = occupyReg ctx
        c1 = compileExpr ctx e1 reg
        c2 = compileExpr ctx2 e2 reg2

-----------------------------------------------------------------------------
-- variable instructions
-----------------------------------------------------------------------------

-- loads variable data from memory to a register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = arpToReg ++ valToReg
    where 
        (depth, offset) = varPos ctx name
        (reg2, ctx2) = occupyReg ctx
        arpToReg = depthArp ctx2 depth reg2
        valToReg = loadAI ctx2 reg2 offset reg

-- updates a variable in memory from register
updateVar :: Context -> VarName -> RegAddr -> [Instruction]
updateVar ctx name reg = arpToReg ++ varToMem
    where
        (depth, offset) = varPos ctx name
        (reg2, ctx2) = occupyReg ctx
        arpToReg = depthArp ctx2 depth reg2
        varToMem = storeAI ctx2 reg reg2 offset

-- updates a variable in memory by value
updateVarImm :: Context -> VarName -> Integer -> [Instruction]
updateVarImm ctx name val = 
    (loadImm val reg2) : (updateVar ctx2 name reg2)
    where (reg2, ctx2) = occupyReg ctx

-----------------------------------------------------------------------------

-- finds a variable position in memory
varPos :: Context -> VarName -> VarPos
varPos (Ctx (scopeId, _) scopeMap _) name = varPos
    where 
        (varMap, _) = fromJust $ Map.lookup scopeId scopeMap
        (varPos, _) = fromJust $ Map.lookup name varMap

-- finds ARP at a certain depth
depthArp :: Context -> Depth -> RegAddr -> [Instruction]
depthArp ctx depth reg = loadArp ctx (depth - scopeDepth) reg
    where (_, scopeDepth) = scope ctx

-- loads an ARP by a depth difference
loadArp :: Context -> Depth -> RegAddr -> [Instruction]
loadArp _   0     reg = [copyReg regDP reg]
loadArp ctx depth reg = [] -- TODO

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

copyReg :: RegAddr -> RegAddr -> Instruction
copyReg reg1 reg2 = Compute Add reg0 reg1 reg2

-- loads value to a register 
loadImm :: Integer -> RegAddr -> Instruction
loadImm val reg = Load (ImmValue $ fromInteger val) reg

-- reverses boolean value in a register
reverseBool :: Context -> RegAddr -> [Instruction]
reverseBool ctx reg = [Compute Equal reg reg0 reg]

-----------------------------------------------------------------------------
-- memory management
-----------------------------------------------------------------------------

-- increments a variable in memory
incrMem :: Context -> VarName -> [Instruction]
incrMem ctx name = addMem ctx name 1

-- adds value to a variable in memory
addMem :: Context -> VarName -> Integer -> [Instruction]
addMem ctx name val =
       loadVar ctx2 name reg2
    ++ [loadImm val reg3, Compute Add reg2 reg3 reg2] 
    ++ updateVar ctx2 name reg2
    where 
        (reg2, ctx2) = occupyReg ctx
        reg3 = findReg ctx2

-- loads from memory with an offset
loadAI :: Context -> RegAddr -> Offset -> RegAddr -> [Instruction]
loadAI ctx reg1 offset reg2 = 
    [
        Load (ImmValue offset) reg2,
        Compute Add reg1 reg2 reg2,
        Load (IndAddr reg2) reg2
    ]

-- stores to memory with an offset
storeAI :: Context -> RegAddr -> RegAddr -> Offset -> [Instruction]
storeAI ctx reg1 reg2 offset =
    [
        Load (ImmValue offset) reg3,
        Compute Add reg3 reg2 reg2,
        Store reg1 (IndAddr reg2)
    ]
    where reg3 = findReg ctx

-----------------------------------------------------------------------------
-- IO instructions
-----------------------------------------------------------------------------

-- generates code to print a string
writeString :: Context -> String -> [Instruction]
writeString ctx str = concat $ map (writeChar reg) str
    where reg = findReg ctx

-- generates code to print a single character
writeChar :: RegAddr -> Char -> [Instruction]
writeChar reg c = 
    [ 
        Load (ImmValue $ ord c) reg, 
        WriteInstr reg charIO 
    ]