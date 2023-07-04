{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile, precompile) where

import Sprockell
import SprockellExt
import PreCompiler
import PostParser
import Parser
import Debug.Trace
import Data.Maybe
import Data.Char (ord)
import qualified Data.Map as Map

-- compiles string into the SpriL language
compile :: String -> [Instruction]
compile code = initArp ++ progASM ++ [EndProg]
    where
        prog            = postScript $ tryParse script code
        progASM         = compileScript (initCtx prog) prog

-----------------------------------------------------------------------------
-- script compilation
-----------------------------------------------------------------------------

compileScript :: Context -> Script -> [Instruction]
compileScript ctx [] = []
compileScript ctx (x:xs) = (compileStmt ctx x) 
    -- ++ (compileScript ctx xs)
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
    VarDecl (name, _) (Just expr)  -> updateVar ctx name 0 expr
    VarAssign name expr            -> updateVar ctx name 0 expr
    ArrInsert name idx expr        -> updateVar ctx name idx expr

    InScope script   -> childScope ctx script
    Action expr      -> voidExpr ctx expr

    FunDef name args returnType script 
        -> putPC ctx ("_f_" ++ name)
        ++ jumpOver funBlock 
        ++ funBlock
        where
            ctxScp       = childCtx ctx
            (reg2, ctx2) = occupyReg ctxScp
            body         = compileScript ctxScp script
            returnAddr   = loadVar ctx2 "_rtn" reg2
            callerArp    = loadVar ctx2 "_sl" regArp
            funBlock     = body ++ returnAddr
                           ++ callerArp ++ [Jump $ Ind reg2]

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
            elseBody  = peerScope ctx elseScript

childScope :: Context -> Script -> [Instruction]
childScope ctx = putInScope ctx . compileScript (childCtx ctx)

peerScope :: Context -> Script -> [Instruction]
peerScope ctx = putInScope ctx . compileScript (peerCtx ctx)

-- compiles an expression, which result is stored in a variable
updateVar :: Context -> VarName -> Integer -> Expr -> [Instruction]
updateVar ctx name idx expr = case expr of
    Fixed (Arr vals)    -> putArrImm ctx name (intVal <$> vals) 
    expr                -> exprToReg ++ varToMem
    where 
        (reg2, ctx2)    = occupyReg ctx
        exprToReg       = compileExpr ctx2 expr reg2
        varToMem        = putVarAtIdx ctx2 name reg2 idx

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

    Var name  -> loadVar ctx name reg
    Fixed val -> [loadImm (intVal val) reg]

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
        -> loadVar ctx ("_f_" ++ name) reg
        ++ loadArp ctx2 (scopeDepth - depth) reg2
        ++ putVar ctx2 "_arp" reg2
        ++ [copyReg regArp reg2]
        ++ setNextArp ctx2
        ++ putVar funCtx "_sl" reg2
        ++ putPC funCtx "_rtn"
        ++ [Jump $ Ind reg]
        where 
            (reg2, ctx2)        = occupyReg ctx
            (_, scopeDepth, _)  = getScope ctx
            (scopeId, depth)    = fromJust $ Map.lookup name (funMap ctx)
            funCtx              = ctx2 { scopeId = scopeId }

-- translates parsed values to integers
intVal :: Parser.Value -> Integer
intVal (Int val)    = val
intVal (Bool val)   = intBool val
intVal (Char val)   = toInteger $ ord val
intVal (None)       = -1
intVal val = error $ "failed translating value: " ++ show val

-- compiles a skip condition from an expression
skipCond :: Context -> Expr -> [Instruction] -> [Instruction] 
skipCond ctx expr body = let (reg2, ctx2) = occupyReg ctx in
       compileExpr ctx2 expr reg2
    ++ [Compute Equal reg2 reg0 reg2]
    ++ branchOver reg2 body

-- compiles a binary operation
compileBin :: Context -> Expr -> Expr -> RegAddr -> Operator -> [Instruction]
compileBin ctx e1 e2 reg op = 
    c1 ++ c2 ++ [Compute op reg reg2 reg]
    where 
        (reg2, ctx2) = occupyReg ctx
        c1 = compileExpr ctx  e1 reg
        c2 = compileExpr ctx2 e2 reg2