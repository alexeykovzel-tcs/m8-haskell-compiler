{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile) where

import Sprockell
import Common.SprockellExt
import Common.Table (tableToMap)
import PreCompiler
import PostParser
import Parser
import Debug.Trace
import Data.Char (ord)
import qualified Data.Map as Map

-- compiles string into the SpriL language
compile :: String -> [Instruction]
compile code = initDP ++ progASM ++ [EndProg]
    where
        prog     = postParse $ tryParse script code
        progASM  = compileScript (initCtx prog) prog

-----------------------------------------------------------------------------
-- script compilation
-----------------------------------------------------------------------------

compileScript :: Context -> Script -> [Instruction]
compileScript ctx [] = []
compileScript ctx (x:xs) = (compileStmt ctx x) ++ (compileScript ctx xs)

compileStmt :: Context -> Statement -> [Instruction]
compileStmt ctx stmt = case stmt of

    VarDecl (name, _) Nothing      -> []
    VarDecl (name, _) (Just expr)  -> updateVar ctx name 0 expr
    VarAssign name expr            -> updateVar ctx name 0 expr
    ArrInsert name idx expr        -> updateVar ctx name idx expr
    InScope script                 -> simpleScope ctx script
    Action expr                    -> voidExpr ctx expr

    WhileLoop expr script -> cond ++ body
        where
            cond  = skipCond ctx expr body
            body  = simpleScope ctx script ++ jumpBack [body, cond]

    Condition expr ifScript Nothing -> cond ++ ifBody
        where
            cond    = skipCond ctx expr ifBody
            ifBody  = simpleScope ctx ifScript

    Condition expr ifScript (Just elseScript) 
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = simpleScope ctx ifScript ++ jumpOver elseBody
            elseBody  = elseScope ctx elseScript

-- sets context scope for a script
simpleScope :: Context -> Script -> [Instruction]
simpleScope ctx = putInScope ctx . compileScript (inScopeCtx ctx)

-- sets context scope for the "else" script
elseScope :: Context -> Script -> [Instruction]
elseScope ctx = putInScope ctx . compileScript (inScopeCtxElse ctx)

-- compiles an expression, which result is stored in a variable
updateVar :: Context -> VarName -> Integer -> Expr -> [Instruction]
updateVar ctx name idx expr = case expr of
    Fixed (Arr vals) -> putArrImm ctx name (intVal <$> vals) 
    Fixed (Text text) -> putArrImm ctx name (toInteger <$> ord <$> text)
    expr -> exprToReg ++ varToMem
    where 
        (reg2, ctx2) = occupyReg ctx
        exprToReg    = compileExpr ctx2 expr reg2
        varToMem     = putVar ctx2 name reg2 idx

-- compiles an expression, which result is ignored
voidExpr :: Context -> Expr -> [Instruction]
voidExpr ctx expr = 
    let (reg2, ctx2) = occupyReg ctx 
    in compileExpr ctx2 expr reg2 

-----------------------------------------------------------------------------
-- expression compilation
-----------------------------------------------------------------------------

compileExpr :: Context -> Expr -> RegAddr -> [Instruction]
compileExpr ctx expr reg = case expr of

    Parser.Add e1 e2 -> compileBin ctx e1 e2 reg Sprockell.Add
    Parser.Sub e1 e2 -> compileBin ctx e1 e2 reg Sprockell.Sub

    Mult    e1 e2 -> compileBin ctx e1 e2 reg Mul
    Eq      e1 e2 -> compileBin ctx e1 e2 reg Equal
    MoreEq  e1 e2 -> compileBin ctx e1 e2 reg GtE
    LessEq  e1 e2 -> compileBin ctx e1 e2 reg LtE
    More    e1 e2 -> compileBin ctx e1 e2 reg Gt
    Less    e1 e2 -> compileBin ctx e1 e2 reg Lt
    Both    e1 e2 -> compileBin ctx e1 e2 reg And
    OneOf   e1 e2 -> compileBin ctx e1 e2 reg Or

    Var name  -> loadVar ctx name reg 0
    Fixed val -> [loadImm (intVal val) reg]

    ArrAccess name idx -> loadVar ctx name reg idx

    Ternary expr1 expr2 expr3
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr1 ifBody
            ifBody    = compileExpr ctx expr2 reg ++ jumpOver elseBody
            elseBody  = compileExpr ctx expr3 reg

    FunCall "thread_id" [Var name]
        -> putVar ctx name regSprID 0

    FunCall "thread_create" [Fixed (Int threadId)]
        -> printStrLn ctx ("create thread: " ++ show threadId)

    FunCall "thread_join" [Fixed (Int threadId)]
        -> printStrLn ctx ("join thread: " ++ show threadId)
 
    FunCall "print" [Fixed (Text msg)]
        -> printStrLn ctx msg

    FunCall "print" [expr]
        -> compileExpr ctx expr reg 
        ++ [WriteInstr reg numberIO]

-- translates parsed values to integers
intVal :: Parser.Value -> Integer
intVal (Int val) = val
intVal (Bool val) = intBool val
intVal (None) = -1
intVal val = error $ "failed translating value: " ++ show val

-- compiles a skip condition from an expression
skipCond :: Context -> Expr -> [Instruction] -> [Instruction] 
skipCond ctx expr body = let (reg2, ctx2) = occupyReg ctx in
       compileExpr ctx2 expr reg2
    ++ notBool ctx2 reg2
    ++ branchOver reg2 body

-- compiles a binary operation
compileBin :: Context -> Expr -> Expr -> RegAddr -> Operator -> [Instruction]
compileBin ctx e1 e2 reg op = 
    c1 ++ c2 ++ [Compute op reg reg2 reg]
    where 
        (reg2, ctx2) = occupyReg ctx
        c1 = compileExpr ctx  e1 reg
        c2 = compileExpr ctx2 e2 reg2