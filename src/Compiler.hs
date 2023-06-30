{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile) where

import Sprockell
import Common.SprockellExt
import Common.Table (tableToMap)
import PreCompiler
import PostParser
import qualified Parser as AST
import qualified Data.Map as Map

-- compiles string into the SpriL language
compile :: String -> [Instruction]
compile code = initDP ++ progASM ++ [EndProg]
    where
        prog     = postParse $ AST.tryParse AST.script code
        progASM  = compileScript (initCtx prog) prog

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
    AST.InScope script                 -> simpleScope ctx script
    AST.Action expr                    -> compileVoidExpr ctx expr

    AST.WhileLoop expr script -> cond ++ body
        where
            cond  = skipCond ctx expr body
            body  = simpleScope ctx script ++ jumpBack [body, cond]

    AST.Condition expr ifScript Nothing -> cond ++ ifBody
        where
            cond    = skipCond ctx expr ifBody
            ifBody  = simpleScope ctx ifScript

    AST.Condition expr ifScript (Just elseScript) 
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = simpleScope ctx ifScript ++ jumpOver elseBody
            elseBody  = elseScope ctx elseScript

-- sets context scope for a script
simpleScope :: Context -> AST.Script -> [Instruction]
simpleScope ctx = inScope ctx . compileScript (inScopeCtx ctx)

-- sets context scope for the "else" script
elseScope :: Context -> AST.Script -> [Instruction]
elseScope ctx = inScope ctx . compileScript (inScopeCtxElse ctx)

-- compiles an expression, which result is stored in a variable
compileVarExpr :: Context -> AST.VarName -> AST.Expr -> [Instruction]
compileVarExpr ctx name expr = exprToReg ++ varToMem
    where 
        (reg2, ctx2) = occupyReg ctx
        exprToReg    = compileExpr ctx2 expr reg2
        varToMem     = updateVar ctx2 name reg2

-- compiles an expression, which result is ignored
compileVoidExpr :: Context -> AST.Expr -> [Instruction]
compileVoidExpr ctx expr = 
    let (reg2, ctx2) = occupyReg ctx 
    in compileExpr ctx2 expr reg2 

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

    -- loads variable value
    AST.Var name -> loadVar ctx name reg

    -- TODO: Arrays, String and None
    AST.Fixed (AST.Int  val)  -> [loadImm val reg]
    AST.Fixed (AST.Bool val)  -> [Load (ImmValue $ intBool val) reg]
    AST.Fixed (AST.None)      -> [Load (ImmValue $ -1) reg]
    AST.Fixed (AST.Arr vals)  -> []
    AST.Fixed (AST.Text text) -> []

    AST.Ternary expr1 expr2 expr3
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr1 ifBody
            ifBody    = compileExpr ctx expr2 reg ++ jumpOver elseBody
            elseBody  = compileExpr ctx expr3 reg

    -- embedded functions
    AST.FunCall "thread_create" ((AST.Fixed (AST.Int threadId)):_)
        -> printStrLn ctx ("create thread: " ++ show threadId)

    AST.FunCall "thread_join" ((AST.Fixed (AST.Int threadId)):_)
        -> printStrLn ctx ("join thread: " ++ show threadId)
 
    AST.FunCall "print" ((AST.Fixed (AST.Text msg)):_)
        -> printStrLn ctx msg

    AST.FunCall "print" (expr:_)
        -> compileExpr ctx expr reg 
        ++ [WriteInstr reg numberIO]

-- compiles a skip condition from an expression
skipCond :: Context -> AST.Expr -> [Instruction] -> [Instruction] 
skipCond ctx expr body = let (reg2, ctx2) = occupyReg ctx in
       compileExpr ctx2 expr reg2
    ++ notBool ctx2 reg2
    ++ branchOver reg2 body

-- compiles a binary operation
compileBin :: Context -> AST.Expr -> AST.Expr -> RegAddr -> Operator -> [Instruction]
compileBin ctx e1 e2 reg op = 
    c1 ++ c2 ++ [Compute op reg reg2 reg]
    where 
        (reg2, ctx2) = occupyReg ctx
        c1 = compileExpr ctx  e1 reg
        c2 = compileExpr ctx2 e2 reg2