{-# LANGUAGE FlexibleInstances #-}

module CodeGen where

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
compile ctx prog = preCompile ++ compileRec ctx prog 

-- TODO: Build "main" activation record
preCompile :: [Instruction]
preCompile = [Load (ImmValue 0) regArp]

compileRec :: Context -> AST.Script -> [Instruction]
compileRec ctx [] = [EndProg]
compileRec ctx (x:xs) = (compileStmt ctx x) ++ (compileRec ctx xs)

compileStmt :: Context -> AST.Statement -> [Instruction]
compileStmt ctx stmt = case stmt of
    AST.VarDecl (name, _) Nothing      -> []
    AST.VarDecl (name, _) (Just expr)  -> compileVar ctx name expr
    AST.VarAssign name expr            -> compileVar ctx name expr
    AST.Action expr                    -> compileExpr ctx2 expr reg2
    where 
        (reg2, ctx2) = occupyReg ctx

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

    -- put integer to a register
    AST.Fixed (AST.Int val) -> 
        [Load (ImmValue $ fromInteger val) reg]

    AST.FunCall "printHello" _ -> writeString ctx "Hello, World!\n"
    AST.FunCall "print" (e:es) -> compileExpr ctx e reg 
        ++ [WriteInstr reg numberIO]

-----------------------------------------------------------------------------

compileBin :: Context -> AST.Expr -> AST.Expr 
           -> RegAddr -> Operator -> [Instruction]

compileBin ctx e1 e2 reg op = 
    c1 ++ c2 ++ [Compute op reg reg2 reg]
    where 
        (reg2, ctx2) = occupyReg ctx
        c1 = compileExpr ctx e1 reg
        c2 = compileExpr ctx2 e2 reg2

-----------------------------------------------------------------------------
-- manipulations with variables
-----------------------------------------------------------------------------

-- loads variable data to a register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = arpToReg ++ valToReg
    where 
        (depth, offset) = varCoord ctx name
        (reg2, ctx2) = occupyReg ctx
        arpToReg = depthArp ctx2 depth reg2
        valToReg = loadAI ctx2 reg2 offset reg

-- updates a variable in memory
updateVar :: Context -> VarName -> RegAddr -> [Instruction]
updateVar ctx name reg = arpToReg ++ varToMem
    where
        (depth, offset) = varCoord ctx name
        (reg2, ctx2) = occupyReg ctx
        arpToReg = depthArp ctx2 depth reg2
        varToMem = storeAI ctx2 reg reg2 offset

-----------------------------------------------------------------------------

-- finds ARP at a certain depth
depthArp :: Context -> Depth -> RegAddr -> [Instruction]
depthArp ctx depth reg = loadArp ctx depthDiff reg
    where 
        depthDiff = depth - scopeDepth
        (_, scopeDepth) = scope ctx

-- finds a variable coordinate in memory
varCoord :: Context -> VarName -> VarCoord
varCoord (Ctx (scopeId, _) _ varMap _) name = varCoord
    where 
        scopeVarMap = fromJust $ Map.lookup scopeId varMap
        varCoord    = fromJust $ Map.lookup name scopeVarMap

-- loads an ARP by a depth difference
loadArp :: Context -> Depth -> RegAddr -> [Instruction]
loadArp _   0     reg = [Compute Add reg0 regArp reg]
loadArp ctx 1     reg = loadAI ctx regArp (-1) reg
loadArp ctx depth reg = upperArps ++ prevArp
    where 
        upperArps = loadArp ctx (depth - 1) reg
        prevArp = loadAI ctx reg (-1) reg

-----------------------------------------------------------------------------
-- register management
-----------------------------------------------------------------------------

-- reg0         always zero
-- regSprID     contains the sprockellID
-- regSP        stack pointer
-- regPC        program counter

-- Reserve register A for ARP
regArp = regA

-- registers for free usage
userRegs = [regB, regC, regD, regE, regF]

occupyReg :: Context -> (RegAddr, Context)
occupyReg (Ctx s f v (r:rs)) = (r, Ctx s f v rs)

freeReg :: Context -> RegAddr
freeReg ctx = let (r:_) = freeRegs ctx in r

-----------------------------------------------------------------------------
-- helpers
-----------------------------------------------------------------------------

loadAI :: Context -> RegAddr -> Offset -> RegAddr -> [Instruction]
loadAI ctx reg1 offset reg2 = 
    [
        Load (ImmValue offset) reg2,
        Compute Add reg1 reg2 reg2,
        Load (IndAddr reg2) reg2
    ]

storeAI :: Context -> RegAddr -> RegAddr -> Offset -> [Instruction]
storeAI ctx reg1 reg2 offset =
    [
        Load (ImmValue offset) reg3,
        Compute Add reg3 reg2 reg2,
        Store reg1 (IndAddr reg2)
    ]
    where reg3 = freeReg ctx

-- Generate code to print a string
writeString :: Context -> String -> [Instruction]
writeString ctx str = concat $ map (writeChar reg) str
    where reg = freeReg ctx

-- Generate code to print a single character
writeChar :: RegAddr -> Char -> [Instruction]
writeChar reg c = 
    [ 
        Load (ImmValue $ ord c) reg, 
        WriteInstr reg charIO 
    ]