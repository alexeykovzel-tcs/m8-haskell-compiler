module CodeGen where

import Sprockell
import Elaborator
import Data.Maybe
import Parser (VarName)
import qualified Parser as AST
import qualified Data.Map as Map

data Context = Ctx {
    scope    :: Scope, 
    funMap   :: FunMap,
    varMap   :: VarMap, 
    freeRegs :: [RegAddr]
}

data Addr 
    = Mem AddrImmDI 
    | Reg RegAddr

-----------------------------------------------------------------------------
-- loading a variable
-----------------------------------------------------------------------------

-- finds a variable coordinate in memory
varCoord :: Context -> VarName -> VarCoord
varCoord (Ctx (scopeId, _) _ varMap _) varName = varCoord
    where 
        scopeVarMap = fromJust $ Map.lookup scopeId varMap
        varCoord    = fromJust $ Map.lookup varName scopeVarMap

-- loads an ARP by the given depth 
loadArp :: Context -> Depth -> RegAddr -> [Instruction]
loadArp _ 0 reg        = [Compute Add reg0 regArp reg]
loadArp ctx 1 reg      = loadAI ctx regArp (-4) reg
loadArp ctx depth reg  = upperArps ++ prevArp
    where 
        upperArps = loadArp ctx (depth - 1) reg
        prevArp   = loadAI ctx reg (-4) reg

-- loads a variable data to the register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = applyDepth ++ applyOffset
    where
        applyDepth      = loadArp ctx (depth - scopeDepth) reg
        applyOffset     = loadAI ctx reg offset reg
        (depth, offset) = varCoord ctx name
        (_, scopeDepth) = scope ctx

-----------------------------------------------------------------------------
-- register management
-----------------------------------------------------------------------------

-- reg0         always zero
-- regSprID     contains the sprockellID
-- regSP        stack pointer
-- regPC        program counter

-- register containing ARP
regArp = regA

-- registers for free use
userRegs = [regB, regC, regD, regE, regF]

freeReg :: Context -> RegAddr
freeReg ctx = let (r:_) = freeRegs ctx in r

occupyReg :: Context -> (RegAddr, Context)
occupyReg (Ctx s f v (r:rs)) = (r, Ctx s f v rs)

-----------------------------------------------------------------------------
-- expression compilation
-----------------------------------------------------------------------------

compileExpr :: Context -> AST.Expr -> RegAddr -> [Instruction]

-- Only works with integers for now
compileExpr ctx (AST.Fixed (AST.Int val)) reg 
    = [Load (ImmValue $ fromInteger val) reg]

-- loading a variable
compileExpr ctx (AST.Var name) reg = loadVar ctx name reg

-- binary operations
compileExpr ctx (AST.Mult    e1 e2) reg = compileBin ctx e1 e2 Mul   reg 
compileExpr ctx (AST.Add     e1 e2) reg = compileBin ctx e1 e2 Add   reg 
compileExpr ctx (AST.Sub     e1 e2) reg = compileBin ctx e1 e2 Sub   reg 
compileExpr ctx (AST.Eq      e1 e2) reg = compileBin ctx e1 e2 Equal reg 
compileExpr ctx (AST.MoreEq  e1 e2) reg = compileBin ctx e1 e2 GtE   reg 
compileExpr ctx (AST.LessEq  e1 e2) reg = compileBin ctx e1 e2 LtE   reg 
compileExpr ctx (AST.More    e1 e2) reg = compileBin ctx e1 e2 Gt    reg 
compileExpr ctx (AST.Less    e1 e2) reg = compileBin ctx e1 e2 Lt    reg 
compileExpr ctx (AST.Both    e1 e2) reg = compileBin ctx e1 e2 And   reg 
compileExpr ctx (AST.OneOf   e1 e2) reg = compileBin ctx e1 e2 Or    reg 

compileBin :: Context -> AST.Expr -> AST.Expr -> Operator -> RegAddr -> [Instruction]
compileBin ctx e1 e2 op reg = left ++ right ++ computeOp
    where 
        left = compileExpr ctx e1 reg
        right = compileExpr newCtx e2 tempReg
        computeOp = [Compute op reg tempReg reg]
        (tempReg, newCtx) = occupyReg ctx

-- compileExpr ctx (AST.FunCall name args) reg = []
-- compileExpr ctx (AST.StructDecl name args) reg = []
-- compileExpr ctx (AST.Ternary cond e1 e2) reg = []
-- compileExpr ctx (AST.Lambda args script) reg = []

-----------------------------------------------------------------------------
-- testing expressions

testExpr :: String -> IO()
testExpr str = run [prog ++ [WriteInstr regB numberIO, EndProg]]
    where
        expr = AST.tryParse AST.expr str
        prog = compileExpr ctx expr regB
        ctx = Ctx (0, 1) Map.empty Map.empty [regC, regD, regE, regF]

-----------------------------------------------------------------------------
-- statement compilation
-----------------------------------------------------------------------------

-- ...

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