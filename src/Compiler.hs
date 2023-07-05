{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile, precompile) where

import Sprockell
import SprockellExt
import PreCompiler
import Parser
import Debug.Trace
import Data.Maybe
import Data.Char (ord)
import qualified Data.Map as Map

-- compiles a string into the SpriL language
compile :: String -> [Instruction]
compile code = initArp ++ progASM ++ [EndProg]
    where
        prog            = postScript $ parseWith script code
        progASM         = compileScript (initCtx prog) prog

-----------------------------------------------------------------------------
-- script compilation
-----------------------------------------------------------------------------

compileScript :: Context -> Script -> [Instruction]
compileScript ctx [] = []
compileScript ctx (x:xs) = (compileStmt ctx x) 
    ++ (compileScript (toPeerCtx ctx x) xs)

toPeerCtx :: Context -> Statement -> Context
toPeerCtx ctx stmt = case stmt of
    InScope _               -> nextPeer ctx
    FunDef _ _ _ _          -> nextPeer ctx
    WhileLoop _ _           -> nextPeer ctx
    Condition _ _ Nothing   -> nextPeer ctx
    Condition _ _ (Just _)  -> nextPeer $ nextPeer ctx
    stmt                    -> ctx

compileStmt :: Context -> Statement -> [Instruction]
compileStmt ctx stmt = case stmt of

    VarDecl (name, _) Nothing      -> []
    VarDecl (name, _) (Just expr)  -> updateVar ctx name expr
    VarAssign name expr            -> updateVar ctx name expr
    ArrInsert name idx expr        -> updateVarAtIdx ctx name idx expr

    InScope script   -> childScope ctx script
    Action expr      -> compileVoidExpr ctx expr

    FunDef name args returnType script 
        -> funAddr ++ jumpOver block ++ block
        where
            funAddr = putPC ctx ("_f_" ++ name)
            ctxScp  = childCtx ctx
            body    = compileScript ctxScp script
            block   = body ++ prepReturn ctxScp

    ReturnVal expr 
        -> compileExpr ctx2 expr reg2
        ++ putVar ctx2 "_rtn_val" reg2
        -- ++ prepReturn ctx
        where 
            (reg2, ctx2) = occupyReg ctx

    WhileLoop expr script -> cond ++ body
        where
            cond      = skipCond ctx expr body
            body      = childScope ctx script ++ jumpBack [body, cond]

    Condition expr ifScript Nothing -> cond ++ ifBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = childScope ctx ifScript

    Condition expr ifScript (Just elseScript) 
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = childScope ctx ifScript ++ jumpOver elseBody
            elseBody  = putInScope ctx $ compileScript (peerCtx ctx) elseScript

prepReturn :: Context -> [Instruction]
prepReturn ctx = returnAddr ++ callerArp ++ [Jump $ Ind reg2]
    where 
        returnAddr   = loadVar ctx2 "_rtn_addr" reg2
        callerArp    = loadVar ctx2 "_link" regArp
        (reg2, ctx2) = occupyReg ctx

-- puts a script in the child context
childScope :: Context -> Script -> [Instruction]
childScope ctx = putInScope ctx . compileScript (childCtx ctx)

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

    Var name  -> loadVar ctx name reg
    Fixed val -> [loadImm (intVal val) reg]
    Neg expr  -> compileExpr ctx expr reg 
              ++ [Compute Sprockell.Sub reg0 reg reg]

    ArrAccess name idx -> loadVarAtIdx ctx name reg idx

    Ternary expr1 expr2 expr3
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr1 ifBody
            ifBody    = compileExpr ctx expr2 reg ++ jumpOver elseBody
            elseBody  = compileExpr ctx expr3 reg

    FunCall "set_process_id" [Var name]
        -> putVar ctx name regSprID
 
    FunCall "print_str" [Fixed msg]
        -> printStrLn ctx (show msg)

    FunCall "print_str" [Var name]
        -> applyArr ctx name (\reg -> [WriteInstr reg charIO])
        ++ printChar reg '\n'

    FunCall "print" [expr]
        -> compileExpr ctx expr reg 
        ++ [WriteInstr reg numberIO] 

    FunCall name args
        -- get the function's address
        -> loadVar ctx ("_f_" ++ name) reg
        -- get the function's ARP based on its depth
        ++ loadArp ctx2 (depth - funDepth) reg2
        ++ putVar ctx2 "_arp" reg2
        ++ [copyReg regArp reg2]
        -- set the function's context (incl. ARP)
        ++ setNextArp ctx2
        -- save the caller's ARP 
        ++ putVar funCtx "_link" reg2
        -- save the caller's arguments
        ++ updateVars funCtx argNames args
        -- save the return address
        ++ putPC funCtx "_rtn_addr"
        ++ [Jump $ Ind reg]
        -- retrieve the return value
        ++ loadAI ctx regArp size reg
        where 
            (reg2, ctx2)      = occupyReg ctx
            (_, depth, size)  = getScope ctx
            funCtx            = ctx2 { scopeId = scopeId }
            (scopeId, funDepth, argNames) 
                = fromJust $ Map.lookup name (funMap ctx)

-----------------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------------------

-- translates parsed values to integers
intVal :: Parser.Value -> Integer
intVal (Int val)    = val
intVal (Bool val)   = intBool val
intVal (Char val)   = toInteger $ ord val
intVal val = error $ "failed translating value: " ++ show val

-- updates multiple variables from expressions
updateVars :: Context -> [VarName] -> [Expr] -> [Instruction]
updateVars _ [] [] = []
updateVars ctx (name:xs) (expr:ys) 
    =  updateVar ctx name expr
    ++ updateVars ctx xs ys

-- updates a variable from expression
updateVar :: Context -> VarName -> Expr -> [Instruction]
updateVar ctx name expr = updateVarAtIdx ctx name 0 expr

-- saves the expression result in a variable at a given index
updateVarAtIdx :: Context -> VarName -> Integer -> Expr -> [Instruction]
updateVarAtIdx ctx name idx expr = case expr of
    Fixed (Arr vals)    -> putArrImm ctx name (intVal <$> vals) 
    expr                -> exprToReg ++ varToMem
    where 
        (reg2, ctx2)    = occupyReg ctx
        exprToReg       = compileExpr ctx2 expr reg2
        varToMem        = putVarAtIdx ctx2 name reg2 idx

-- compiles a skip condition from an expression
skipCond :: Context -> Expr -> [Instruction] -> [Instruction] 
skipCond ctx expr body = let (reg2, ctx2) = occupyReg ctx in
       compileExpr ctx2 expr reg2
    ++ [Compute Equal reg2 reg0 reg2]
    ++ branchOver reg2 body

-- compiles a binary operation
compileBin :: Context -> Expr -> Expr -> RegAddr -> Operator -> [Instruction]
compileBin ctx e1 e2 reg op = let reg2 = findReg ctx
    in compileExpr ctx e1 reg
    ++ [Push reg]
    ++ compileExpr ctx e2 reg
    ++ [Pop reg2]
    ++ [Compute op reg2 reg reg]

-- compiles an expression, which result is ignored
compileVoidExpr :: Context -> Expr -> [Instruction]
compileVoidExpr ctx expr = 
    let (reg2, ctx2) = occupyReg ctx 
    in compileExpr ctx2 expr reg2 