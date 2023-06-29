{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile) where

import Sprockell
import Elaborator
import Data.Maybe
import Utils.Table (tableToMap)
import Utils.Sprockell
import Debug.Trace (trace)
import qualified Parser as AST
import qualified Data.Map as Map

compile :: String -> [Instruction]
compile code = initDP ++ progASM ++ [EndProg]
    where
        prog    = AST.tryParse AST.script code
        progASM = compileScript (initCtx prog) prog

initCtx :: AST.Script -> Context
initCtx prog = Ctx (0, 1) scopeMap path userRegs
    where
        (varTable, path) = scopeCtx prog
        scopeMap = toScopeMap (Map.fromList path) (tableToMap varTable)

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
    AST.Action expr                    -> compileExpr ctx2 expr reg2
    
    AST.WhileLoop expr script 
        -> cond ++ branch ++ body ++ jumpBack
        where
            cond      = compileCond ctx2 expr reg2 
            branch    = [Branch reg2 $ Rel relSkip]
            relSkip   = (length body) + 2
            body      = inScope ctx $ compileScript (inScopeCtx ctx) script
            jumpBack  = [Jump $ Rel relBack]
            relBack   = - (length body) - (length branch) - (length cond)
    
    -- TODO: Update ARP
    AST.ForLoop (iter, _) (AST.IterRange from to) script 
        -> inScope ctx $ saveIter ++ loadIter ++ branch 
                          ++ body ++ incrIter ++ jumpBack
        where
            ctxScp    = inScopeCtx ctx
            saveIter  = updateVarImm ctxScp iter from
            loadIter  = loadVar ctxScp2 iter reg2
            branch    = [loadImm to reg3,             -- load 'to'
                         Compute Gt reg2 reg3 reg3,   -- check if 'i' > 'to'
                         Branch reg3 $ Rel relSkip]   -- skip if true
            body      = compileScript ctxScp script
            incrIter  = incrMem ctxScp iter
            jumpBack  = [Jump $ Rel relBack]
            reg3      = findReg ctxScp2
            relSkip   = (length body) + (length incrIter) + 2
            relBack   = - (length body) - (length incrIter) 
                        - (length loadIter) - (length branch)
            (reg2, ctxScp2) = occupyReg ctxScp

    -- compileScript does not store last scope... 
    AST.Condition expr ifScript elseScript 
        -> cond ++ branch ++ ifBody ++ elseBody
        where
            ctxScp    = inScopeCtx ctx
            ctxScp2   = ctx
            cond      = compileCond ctx2 expr reg2
            branch    = [Branch reg2 $ Rel $ length ifBody + 1]
            ifBody    = inScope ctx (compileScript ctxScp ifScript) ++ skipElse
            elseBody  = maybe [] (inScope ctx . compileScript ctxScp2) elseScript
            skipElse  = maybe [] (\_ -> jumpElse) elseScript
            jumpElse  = [Jump $ Rel $ length elseBody + 1]

    where (reg2, ctx2) = occupyReg ctx

compileCond :: Context -> AST.Expr -> RegAddr -> [Instruction]
compileCond ctx expr reg = 
    (compileExpr ctx expr reg) ++ (notBool ctx reg)

compileVarExpr :: Context -> VarName -> AST.Expr -> [Instruction]
compileVarExpr ctx name expr = exprToReg ++ varToMem
    where 
        (reg2, ctx2) = occupyReg ctx
        exprToReg    = compileExpr ctx2 expr reg2
        varToMem     = updateVar ctx2 name reg2

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
    AST.Fixed (AST.Int  val)  -> [loadImm val reg]
    AST.Fixed (AST.Bool val)  -> [Load (ImmValue $ intBool val) reg]
    AST.Fixed (AST.None)      -> [Load (ImmValue $ -1) reg]
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

-- compiles a binary operation
compileBin :: Context -> AST.Expr -> AST.Expr 
           -> RegAddr -> Operator -> [Instruction]

compileBin ctx e1 e2 reg op = 
    c1 ++ c2 ++ [Compute op reg reg2 reg]
    where 
        (reg2, ctx2) = occupyReg ctx
        c1 = compileExpr ctx  e1 reg
        c2 = compileExpr ctx2 e2 reg2