module CodeGen ( compile ) where

import Sprockell
import Parser

regs = [regB, regC, regD, regE, regF]

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