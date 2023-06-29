{-# LANGUAGE FlexibleInstances #-}

module Compiler where

import Sprockell
import Elaborator
import Data.Maybe
import Data.Char
import Table (tableToMap)
import Parser (VarName, FunName)
import qualified Parser as AST
import qualified Data.Map as Map

type VarMap = Map.Map ScopeID (Map.Map VarName VarPos) 
type Scope = (ScopeID, Depth)

data Context = Ctx {
    scope :: Scope,
    varMap :: VarMap,
    freeRegs  :: [RegAddr]
}

compile :: String -> [Instruction]
compile code = preCompile ++ compileScript ctx prog ++ [EndProg]
    where
        prog = AST.tryParse AST.script code
        ctx = Ctx (0, 1) (tableToMap $ varTable prog) userRegs

-- TODO: Build "main" activation record
preCompile :: [Instruction]
preCompile = [Load (ImmValue 0) regArp]

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
        -> cond ++ notCond ++ branch ++ body ++ jumpBack
        where
            cond      = compileExpr ctx2 expr reg2 
            notCond   = reverseBool ctx2 reg2
            branch    = [Branch reg2 $ Rel relSkip]
            body      = compileScript ctx script
            jumpBack  = [Jump $ Rel relBack]
            relSkip   = (length body) + 2
            relBack   = - (length body) - (length branch) 
                        - (length notCond) - (length cond)
    
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
        -> cond ++ notCond ++ branch ++ ifBody ++ elseBody
        where
            cond      = compileExpr ctx2 expr reg2
            notCond   = reverseBool ctx2 reg2
            branch    = [Branch reg2 $ Rel $ length ifBody + 1]
            ifBody    = compileScript ctx ifScript ++ skipElse
            elseBody  = maybe [] (compileScript ctx) elseScript
            skipElse  = maybe [] (\_ -> jumpElse) elseScript
            jumpElse  = [Jump $ Rel $ length elseBody + 1]

    -- TODO: Implement these:
    -- AST.FunDef name args returnType script -> []
    -- AST.ReturnVal expr -> []
    -- AST.ArrInsert name idx expr -> []

    where (reg2, ctx2) = occupyReg ctx

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
        -> cond ++ notCond ++ branch ++ ifBody ++ elseBody
        where
            cond      = compileExpr ctx expr reg
            notCond   = reverseBool ctx reg
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
updateVarImm ctx name val = (loadImm val reg2) : (updateVar ctx2 name reg2)
    where (reg2, ctx2) = occupyReg ctx

-----------------------------------------------------------------------------

-- finds ARP at a certain depth
depthArp :: Context -> Depth -> RegAddr -> [Instruction]
depthArp ctx depth reg = loadArp ctx depthDiff reg
    where 
        depthDiff = depth - scopeDepth
        (_, scopeDepth) = scope ctx

-- finds a variable position in memory
varPos :: Context -> VarName -> VarPos
varPos (Ctx (scopeId, _) varMap _) name = result
    where 
        scopeVarMap = fromJust $ Map.lookup scopeId varMap
        result    = fromJust $ Map.lookup name scopeVarMap

-- loads an ARP by a depth difference
loadArp :: Context -> Depth -> RegAddr -> [Instruction]
loadArp _   0     reg = [Compute Add reg0 regArp reg]
loadArp ctx 1     reg = loadAI ctx regArp (-1) reg
loadArp ctx depth reg = upperArps ++ prevArp
    where 
        upperArps = loadArp ctx (depth - 1) reg
        prevArp = loadAI ctx reg (-1) reg

-----------------------------------------------------------------------------
-- register instructions
-----------------------------------------------------------------------------

-- reg0         always zero
-- regSprID     contains the sprockellID
-- regSP        stack pointer
-- regPC        program counter

-- Reserve register A for ARP
regArp = regA

-- registers for free usage
userRegs = [regB, regC, regD, regE, regF]

-- occupies a free register
occupyReg :: Context -> (RegAddr, Context)
occupyReg (Ctx s v (r:rs)) = (r, Ctx s v rs)

-- finds a free register
findReg :: Context -> RegAddr
findReg ctx = let (r:_) = freeRegs ctx in r

-- loads value to a register 
loadImm :: Integer -> RegAddr -> Instruction
loadImm val reg = Load (ImmValue $ fromInteger val) reg

-- reverses boolean value in a register
reverseBool :: Context -> RegAddr -> [Instruction]
reverseBool ctx reg = [Compute Equal reg reg0 reg]

-----------------------------------------------------------------------------
-- memory instructions
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