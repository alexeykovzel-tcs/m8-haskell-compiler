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

-- compiles a script into the SpriL language
compileScript :: Context -> Script -> [Instruction]
compileScript ctx [] = []
compileScript ctx (x:xs) = (compileStmt ctx x) 
    ++ (compileScript (toPeerCtx ctx x) xs)

-- updates context for the next scope
toPeerCtx :: Context -> Statement -> Context
toPeerCtx ctx stmt = case stmt of
    InScope _               -> nextPeer ctx
    FunDef _ _ _ _          -> nextPeer ctx
    WhileLoop _ _           -> nextPeer ctx
    Condition _ _ Nothing   -> nextPeer ctx
    Condition _ _ (Just _)  -> nextPeer $ nextPeer ctx
    stmt                    -> ctx

-- compiles a statement into the SpriL language
compileStmt :: Context -> Statement -> [Instruction]
compileStmt ctx stmt = case stmt of

    -- updates a variable value
    VarDecl (name, _) Nothing      -> []
    VarDecl (name, _) (Just expr)  -> updateVar ctx name expr
    VarAssign name expr            -> updateVar ctx name expr
    ArrInsert name idx expr        -> updateVarAtIdx ctx name idx expr

    -- puts instructions into a new scope
    InScope script -> childScope ctx script

    -- executes expression without saving the result
    Action expr ->
        let (reg2, ctx2) = occupyReg ctx 
        in compileExpr ctx2 expr reg2 

    -- compiles a function definition
    FunDef name args returnType script 
        -> funAddr ++ jumpOver block ++ block
        where
            funAddr = putPC ctx ("_f_" ++ name)
            ctxScp  = childCtx ctx
            body    = compileScript ctxScp script
            block   = body ++ prepReturn ctxScp

    -- saves the function result
    ReturnVal expr 
        -> compileExpr ctx2 expr reg2
        ++ putVar ctx2 "_rtn_val" reg2
        ++ prepReturn ctx -- TODO: Think about it...
        where 
            (reg2, ctx2) = occupyReg ctx

    -- compiles a "while" loop
    WhileLoop expr script -> cond ++ body
        where
            cond      = skipCond ctx expr body
            body      = childScope ctx script ++ jumpBack [body, cond]

    -- compiles an if condition
    Condition expr ifScript Nothing -> cond ++ ifBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = childScope ctx ifScript

    -- compiles an if/else condition
    Condition expr ifScript (Just elseScript) 
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = childScope ctx ifScript ++ jumpOver elseBody
            elseBody  = putInScope ctx $ compileScript (peerCtx ctx) elseScript

-- handles a function return
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

-- compiles an expression into the SpriL language
compileExpr :: Context -> Expr -> RegAddr -> [Instruction]
compileExpr ctx expr reg = case expr of

    Var name  -> loadVar ctx name reg
    Fixed val -> [loadImm (intVal val) reg]

    -- compiles a binary operation
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

    -- executes an expression and reverts its sign
    Neg expr -> compileExpr ctx expr reg 
        ++ [Compute Sprockell.Sub reg0 reg reg]

    -- loads an element from an array
    ArrAccess name idx -> loadVarAtIdx ctx name reg idx

    -- compiles a ternary operator
    Ternary expr1 expr2 expr3
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr1 ifBody
            ifBody    = compileExpr ctx expr2 reg ++ jumpOver elseBody
            elseBody  = compileExpr ctx expr3 reg

    -- stores the current Sprockell id to a variable
    FunCall "set_process_id" [Var name]
        -> putVar ctx name regSprID
 
    -- prints a string directly
    FunCall "print_str" [Fixed msg]
        -> printStrLn ctx (show msg)

    -- prints a string from a variable
    FunCall "print_str" [Var name]
        -> applyArr ctx name (\reg -> [WriteInstr reg charIO])
        ++ printChar reg '\n'

    -- prints the result of an expression
    FunCall "print" [expr]
        -> compileExpr ctx expr reg 
        ++ [WriteInstr reg numberIO] 

    FunCall name args
        -- get function's address
        -> loadVar ctx ("_f_" ++ name) reg
        -- get function's ARP based on its depth
        ++ loadArp ctx2 (depth - funDepth) reg2
        ++ putVar ctx2 "_arp" reg2
        ++ [copyReg regArp reg2]
        -- set function's context (incl. ARP)
        ++ setNextArp ctx2
        -- save caller's ARP 
        ++ putVar funCtx "_link" reg2
        -- save caller's arguments
        ++ updateVars funCtx argNames args
        -- save return address
        ++ putPC funCtx "_rtn_addr"
        ++ [Jump $ Ind reg]
        -- retrieve return value
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

-- updates multiple variables in memory
updateVars :: Context -> [VarName] -> [Expr] -> [Instruction]
updateVars _ [] [] = []
updateVars ctx (name:xs) (expr:ys) 
    =  updateVar ctx name expr
    ++ updateVars ctx xs ys

-- updates a variable in memory
updateVar :: Context -> VarName -> Expr -> [Instruction]
updateVar ctx name expr = updateVarAtIdx ctx name 0 expr

-- updates a variable in memory at the given index
updateVarAtIdx :: Context -> VarName -> Integer -> Expr -> [Instruction]
updateVarAtIdx ctx name idx expr = case expr of
    Fixed (Arr vals)    -> putArrImm ctx name (intVal <$> vals) 
    expr                -> exprToReg ++ varToMem
    where 
        (reg2, ctx2)    = occupyReg ctx
        exprToReg       = compileExpr ctx2 expr reg2
        varToMem        = putVarAtIdx ctx2 name reg2 idx

-- compiles a "skip" condition from an expression
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