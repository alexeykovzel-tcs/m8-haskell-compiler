{-# LANGUAGE FlexibleInstances #-}

module Compiler where

import Sprockell
import Elaborator
import Data.Maybe
import Data.Char
import Parser (VarName)
import qualified Parser as AST
import qualified Data.Map as Map

data Context = Ctx {
    scope :: Scope,          -- current scope 
    funMap :: FunMap,        -- maps function names to ASTs
    varMap :: VarMap,        -- maps variables to coords by scope
    freeRegs  :: [RegAddr]   -- currently free registers
}

-----------------------------------------------------------------------------
-- script compilation
-----------------------------------------------------------------------------

compile :: Context -> AST.Script -> [Instruction]
compile ctx prog = preCompile ++ compileScript ctx prog ++ [EndProg]

-- TODO: Build "main" activation record
preCompile :: [Instruction]
preCompile = [Load (ImmValue 0) regArp]

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
        -> cond ++ reverseCond ++ branch ++ body ++ jumpBack
        where
            cond        = compileExpr ctx2 expr reg2 
            reverseCond = reverseBool ctx2 reg2
            branch      = [Branch reg2 (Rel relSkip)]
            body        = compileScript ctx script
            jumpBack    = [Jump (Rel relBack)]
            relSkip     = (length body) + 2
            relBack     = - (length body) - (length branch) 
                          - (length reverseCond) - (length cond)
    
    AST.ForLoop (i, _) (AST.IterRange from to) script 
        -> saveIter ++ loadIter ++ branch ++ body ++ incrIter ++ jumpBack
        where 
            saveIter = updateVarI ctx i from
            loadIter = loadVar ctx2 i reg2
            branch   = [loadI to reg3,               -- load 'to'
                        Compute Gt reg2 reg3 reg3,   -- check if 'i' > 'to'
                        Branch reg3 (Rel relSkip)]   -- skip if true
            body     = compileScript ctx script
            incrIter = incrMem ctx i
            jumpBack = [Jump (Rel relBack)]
            reg3     = findReg ctx2
            relSkip  = (length body) + (length incrIter) + 2
            relBack  = - (length body) - (length incrIter) 
                       - (length loadIter) - (length branch)

    AST.Condition expr ifScript elseScript 
        -> cond ++ reverseCond ++ branch ++ ifBody ++ elseBody
        where
            cond        = compileExpr ctx2 expr reg2
            reverseCond = reverseBool ctx2 reg2
            ifBody      = compileScript ctx ifScript ++ skipElse
            branch      = [Branch reg2 (Rel $ length ifBody + 1)]
            elseBody    = maybe [] (compileScript ctx) elseScript
            skipElse    = maybe [] (\_ -> jumpElse) elseScript
            jumpElse    = [Jump (Rel $ length elseBody + 1)]

    -- TODO: Implement these:
    AST.StructDef name args                 -> []
    AST.FunDef name args returnType script  -> []
    AST.ArrInsert name idx expr             -> []
    AST.ReturnVal expr                      -> []

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

    -- load variable from memory
    AST.Var name -> loadVar ctx name reg

    -- TODO: Arrays, String and None
    AST.Fixed (AST.Int  val)  -> [Load (ImmValue $ fromInteger val) reg]
    AST.Fixed (AST.Bool val)  -> [Load (ImmValue $ intBool val) reg]
    AST.Fixed (AST.None)      -> []
    AST.Fixed (AST.Arr vals)  -> []
    AST.Fixed (AST.Text text) -> []

    -- embedded functions
    AST.FunCall "printHello" _ 
        -> writeString ctx "Hello, World!\n"
    
    AST.FunCall "print" ((AST.Fixed (AST.Text msg)):_)
        -> writeString ctx (msg ++ "\n")

    AST.FunCall "print" (expr:_)
        -> compileExpr ctx expr reg 
        ++ [WriteInstr reg numberIO]

    -- TODO: Implement this:
    AST.FunCall name args    -> []
    AST.StructDecl name args -> []
    AST.Ternary cond e1 e2   -> []
    AST.Lambda args script   -> []

-----------------------------------------------------------------------------

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
-- variable management
-----------------------------------------------------------------------------

-- loads variable data from memory to a register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = arpToReg ++ valToReg
    where 
        (depth, offset) = varCoord ctx name
        (reg2, ctx2) = occupyReg ctx
        arpToReg = depthArp ctx2 depth reg2
        valToReg = loadAI ctx2 reg2 offset reg

-- updates a variable in memory from register
updateVar :: Context -> VarName -> RegAddr -> [Instruction]
updateVar ctx name reg = arpToReg ++ varToMem
    where
        (depth, offset) = varCoord ctx name
        (reg2, ctx2) = occupyReg ctx
        arpToReg = depthArp ctx2 depth reg2
        varToMem = storeAI ctx2 reg reg2 offset

-- updates a variable in memory by value
updateVarI :: Context -> VarName -> Integer -> [Instruction]
updateVarI ctx name val = (loadI val reg2) : (updateVar ctx2 name reg2)
    where (reg2, ctx2) = occupyReg ctx

-----------------------------------------------------------------------------

-- finds ARP at a certain depth
depthArp :: Context -> Depth -> RegAddr -> [Instruction]
depthArp ctx depth reg = loadArp ctx depthDiff reg
    where 
        depthDiff = depth - scopeDepth
        (_, scopeDepth) = scope ctx

-- finds a variable coordinate in memory
varCoord :: Context -> VarName -> VarCoord
varCoord (Ctx (scopeId, _) _ varMap _) name = result
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
-- instructions with registers
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
occupyReg (Ctx s f v (r:rs)) = (r, Ctx s f v rs)

-- finds a free register
findReg :: Context -> RegAddr
findReg ctx = let (r:_) = freeRegs ctx in r

-- loads value to a register 
loadI :: Integer -> RegAddr -> Instruction
loadI val reg = Load (ImmValue $ fromInteger val) reg

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
    ++ [loadI val reg3, Compute Add reg2 reg3 reg2] 
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