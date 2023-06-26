{-# LANGUAGE FlexibleInstances #-}

module CodeGen (compile) where

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

-----------------------------------------------------------------------------
-- manipulations with variables
-----------------------------------------------------------------------------

-- loads variable data to a register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = arpToReg ++ valToReg
    where 
        (arpToReg, offset) = varArp ctx name reg
        valToReg = loadAI ctx reg offset reg

-- updates a variable in memory
updateVar :: Context -> VarName -> RegAddr -> [Instruction]
updateVar ctx name reg = arpToReg ++ valToMem
    where
        (regTemp, newCtx) = occupyReg ctx
        (arpToReg, offset) = varArp newCtx name regTemp
        valToMem = storeAI newCtx regTemp offset (IndAddr regTemp)

-- finds an ARP from which the variable offsets
varArp :: Context -> VarName -> RegAddr -> ([Instruction], Offset)
varArp ctx name reg = (argToReg, offset)
    where
        argToReg = loadArp ctx (depth - scopeDepth) reg
        (depth, offset) = varCoord ctx name
        (_, scopeDepth) = scope ctx

-- finds a variable coordinate in memory
varCoord :: Context -> VarName -> VarCoord
varCoord (Ctx (scopeId, _) _ varMap _) name = varCoord
    where 
        scopeVarMap = fromJust $ Map.lookup scopeId varMap
        varCoord    = fromJust $ Map.lookup name scopeVarMap

-- loads an ARP by a depth difference
loadArp :: Context -> Depth -> RegAddr -> [Instruction]
loadArp _ 0 reg        = [Compute Add reg0 regArp reg]
loadArp ctx 1 reg      = loadAI ctx regArp (-4) reg
loadArp ctx depth reg  = upperArps ++ arp
    where 
        upperArps = loadArp ctx (depth - 1) reg
        arp = loadAI ctx reg (-4) reg

-----------------------------------------------------------------------------
-- register management
-----------------------------------------------------------------------------

{-
    reg0         always zero
    regSprID     contains the sprockellID
    regSP        stack pointer
    regPC        program counter
-}

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

class Compilable a where
    compile :: Context -> a -> [Instruction]

type ExprToReg = (AST.Expr, RegAddr)

instance Compilable AST.Expr where
    compile ctx expr = compile ctx (expr, reg)
        where reg = freeReg ctx

instance Compilable ExprToReg where

    -- Only works with integers for now
    compile ctx (AST.Fixed (AST.Int val), reg) 
        = [Load (ImmValue $ fromInteger val) reg]

    -- loading a variable
    compile ctx (AST.Var name, reg) = loadVar ctx name reg

    -- binary operations
    compile ctx (AST.Mult    e1 e2, reg) = compile ctx (e1, e2, Mul,   reg) 
    compile ctx (AST.Add     e1 e2, reg) = compile ctx (e1, e2, Add,   reg) 
    compile ctx (AST.Sub     e1 e2, reg) = compile ctx (e1, e2, Sub,   reg) 
    compile ctx (AST.Eq      e1 e2, reg) = compile ctx (e1, e2, Equal, reg) 
    compile ctx (AST.MoreEq  e1 e2, reg) = compile ctx (e1, e2, GtE,   reg) 
    compile ctx (AST.LessEq  e1 e2, reg) = compile ctx (e1, e2, LtE,   reg) 
    compile ctx (AST.More    e1 e2, reg) = compile ctx (e1, e2, Gt,    reg) 
    compile ctx (AST.Less    e1 e2, reg) = compile ctx (e1, e2, Lt,    reg) 
    compile ctx (AST.Both    e1 e2, reg) = compile ctx (e1, e2, And,   reg) 
    compile ctx (AST.OneOf   e1 e2, reg) = compile ctx (e1, e2, Or,    reg) 

    -- compile ctx (AST.FunCall name args, reg) = []
    -- compile ctx (AST.StructDecl name args, reg) = []
    -- compile ctx (AST.Ternary cond e1 e2, reg) = []
    -- compile ctx (AST.Lambda args script, reg) = []

type BinToReg = (AST.Expr, AST.Expr, Operator, RegAddr)

instance Compilable BinToReg where
    compile ctx (expr1, expr2, op, reg) 
        = left ++ right ++ computeOp
        where 
            left = compile ctx (expr1, reg)
            right = compile newCtx (expr2, regTemp)
            computeOp = [Compute op reg regTemp reg]
            (regTemp, newCtx) = occupyReg ctx

-----------------------------------------------------------------------------
-- script compilation
-----------------------------------------------------------------------------

instance Compilable AST.Script where
    compile ctx [] = [EndProg]
    compile ctx (x:xs) = (compile ctx x) ++ (compile ctx xs)

instance Compilable AST.Statement where
    compile ctx (AST.VarDecl (name, _) Nothing)      = []
    compile ctx (AST.VarDecl (name, _) (Just expr))  = compile ctx (name, expr)
    compile ctx (AST.VarAssign name expr)            = compile ctx (name, expr)
    compile ctx (AST.Action expr)                    = compile ctx expr

    -- compile ctx (Condition cond ifScript elseScript) = []
    -- compile ctx (ForLoop i iter script) = []
    -- compile ctx (WhileLoop expr script) = []
    -- compile ctx (FunDef name args type script) = []
    -- compile ctx (ReturnVal expr) = []
    -- compile ctx (StructDef name args) = []
    -- compile ctx (ArrInsert name size expr) = []

type VarExpr = (AST.VarName, AST.Expr)

instance Compilable VarExpr where
    compile ctx (name, expr) = exprToReg ++ (updateVar newCtx name reg)
        where 
            exprToReg = compile newCtx (expr, reg) 
            (reg, newCtx) = occupyReg ctx

-----------------------------------------------------------------------------
-- testing

mainCtx :: [(VarName, VarCoord)] -> Context
mainCtx vars = Ctx (0, 1) funMap varMap regs
    where
        funMap = Map.empty
        varMap = Map.fromList [(0, Map.fromList vars)]
        regs = [regC, regD, regE, regF]

runExpr :: String -> IO()
runExpr str = run [prog ++ [WriteInstr regB numberIO, EndProg]]
    where
        expr = AST.tryParse AST.expr str
        prog = compile (mainCtx []) (expr, regB)

tryScript :: String -> [(VarName, VarCoord)] -> IO()
tryScript str vars = do
    putStrLn ""
    mapM_ print result
    putStrLn ""
    where 
        result = compile (mainCtx vars) ast
        ast = AST.tryParse AST.script str

-- e.g. tryScript "let x = 2;" [("x", (1, 0))]

showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) 
    = show $ localMem $ head $ sprStates systemState

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

storeAI :: Context -> RegAddr -> Offset -> AddrImmDI -> [Instruction]
storeAI ctx reg1 offset target =
    [
        Load (ImmValue offset) reg2,
        Compute Add reg1 reg2 reg2,
        Store reg2 target
    ]
    where reg2 = freeReg ctx