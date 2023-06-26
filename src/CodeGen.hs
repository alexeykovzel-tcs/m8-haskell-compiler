module CodeGen where

import Sprockell
import Elaborator
import Data.Maybe
import Parser (VarName)
import qualified Parser as AST
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- context management
-----------------------------------------------------------------------------

data Context = Ctx {
    scope :: Scope, 
    funMap :: FunMap,
    varMap :: VarMap, 
    freeRegs :: [RegAddr]
}

varCoord :: Context -> VarName -> VarCoord
varCoord (Ctx (scopeId, _) _ varMap _) name = coord
    where 
        scopeVarMap = fromJust $ Map.lookup scopeId varMap
        coord = fromJust $ Map.lookup name scopeVarMap

loadAI :: Context -> RegAddr -> Offset -> RegAddr -> [Instruction]
loadAI ctx regFrom offset regTo = 
    [
        Load (ImmValue offset) tempReg,
        Compute Add regFrom tempReg regTo
    ] 
    where tempReg = freeReg ctx 

loadArp :: Context -> Depth -> RegAddr -> [Instruction]
loadArp _ 0 reg         = [Compute Add reg0 regArp reg]
loadArp ctx 1 reg       = loadAI ctx regArp (-4) reg
loadArp ctx depth reg   = prevShifts ++ nextShift
    where 
        prevShifts  = loadArp ctx (depth - 1) reg
        nextShift   = loadAI ctx reg (-4) reg

loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name addr 
    =  loadArp ctx (depth - scopeDepth) addr 
    ++ (loadAI ctx addr offset addr)
    where
        (depth, offset) = varCoord ctx name
        (_, scopeDepth) = scope ctx
        depthDiff = depth - scopeDepth 

-----------------------------------------------------------------------------
-- register management
-----------------------------------------------------------------------------

-- reg0         always zero
-- regSprID     contains the sprockellID
-- regSP        stack pointer
-- regPC        program counter

regArp = regA

regs = [regB, regC, regD, regE, regF]

freeReg :: Context -> RegAddr
freeReg ctx = let (r:xs) = freeRegs ctx in r

-----------------------------------------------------------------------------

data Addr 
    = Mem AddrImmDI 
    | Reg RegAddr

compileExpr :: AST.Expr -> Addr -> [Instruction]
compileExpr (AST.Fixed val) addr = []

-- compileExpr (AST.Mult expr1 expr2) addr = []
-- compileExpr (AST.Var name) addr = []
-- compileExpr (AST.FunCall name args) addr = []
-- compileExpr (AST.StructDecl name args) addr = []
-- compileExpr (AST.Ternary cond ifExpr elseExpr) addr = []
-- compileExpr (AST.Lambda args script) addr = []
-- compileExpr (AST.Both cond1 cond2) addr = []
-- compileExpr (AST.OneOf cond1 cond2) addr = []
-- compileExpr (AST.Eq expr1 expr2) addr = []
-- compileExpr (AST.MoreOrEq expr1 expr2) addr = []
-- compileExpr (AST.LessOrEq expr1 expr2) addr = []
-- compileExpr (AST.More expr1 expr2) addr = []
-- compileExpr (AST.Less expr1 expr2) addr = []
-- compileExpr (AST.Add expr1 expr2) addr = []
-- compileExpr (AST.Sub expr1 expr2) addr = []