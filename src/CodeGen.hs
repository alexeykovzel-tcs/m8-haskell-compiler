module CodeGen ( compile ) where

import Sprockell
import Parser

-- Sprockell instructions, memory types, etc.:
-- https://github.com/bobismijnnaam/sprockell/blob/master/src/Sprockell/HardwareTypes.hs#L115

compileExpr :: Expr -> [Instruction]
compileExpr (FunCall name args) = []

compileExpr (StructDecl name args) = []

compileExpr (Ternary cond ifExpr elseExpr) = []

compileExpr (Lambda args script) = []

compileExpr (Both cond1 cond2) = []

compileExpr (OneOf cond1 cond2) = []

compileExpr (Eq expr1 expr2) = []

compileExpr (MoreOrEq expr1 expr2) = []

compileExpr (LessOrEq expr1 expr2) = []

compileExpr (More expr1 expr2) = []

compileExpr (Less expr1 expr2) = []

compileExpr (Add expr1 expr2) = []

compileExpr (Sub expr1 expr2) = []

compileExpr (Mult expr1 expr2) = []

compileExpr (Div expr1 expr2) = []

compileExpr (Mod expr1 expr2) = []

compileExpr (Var name) = []

compileExpr (Fixed val) = []

-----------------------------------------------------------------------------
-- register allocation
-----------------------------------------------------------------------------

regs = [regA, regB, regC, regD, regE, regF]

-- reg0         always zero
-- regSprID     contains the sprockellID
-- regSP        stack pointer
-- regPC        program counter

-----------------------------------------------------------------------------
-- examples
-----------------------------------------------------------------------------

-- computes fibonacci sequence
compileFib :: Integer -> [Instruction]
compileFib n = [ 
        Load (ImmValue $ fromInteger n) regE,
        Load (ImmValue 0) regA,
        Load (ImmValue 1) regB,
        Compute Gt regA regE regC,
        Branch regC (Abs 12),
        WriteInstr regA numberIO,
        Compute Add regA regB regA,
        Compute Gt regB regE regC,
        Branch regC (Abs 12),
        WriteInstr regB numberIO,
        Compute Add regA regB regB,
        Jump (Rel (-8)),
        EndProg
    ]