{-# LANGUAGE FlexibleInstances #-}

module Compiler (
    compile, 
    compileRun, 
    compileAST, 
    countWorkers
) where

import Sprockell
import SprockellExt
import PreCompiler
import Parser
import Debug.Trace
import Data.Maybe
import Data.Char (ord)
import qualified Data.Map as Map

compile :: Int -> String -> [Instruction]
compile num code = compileAST num $ parseWith script code

compileRun :: String -> FunName -> [Integer] -> [Instruction]
compileRun code funName args = compileAST 0 $ ast 
    ++ [Action $ FunCall "print" [FunCall funName funArgs]]
    where 
        ast      = parseWith script code
        funArgs  = Fixed <$> Int <$> args

compileAST :: Int -> Script -> [Instruction]
compileAST num ast = startProg ++ prog ++ endProg ctx
    where
        ctx      = initCtx script num
        script   = postScript $ ast
        prog     = compileScript ctx script

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
    Parallel _ _            -> nextPeer ctx
    WhileLoop _ _           -> nextPeer ctx
    Condition _ _ Nothing   -> nextPeer ctx
    Condition _ _ (Just _)  -> nextPeer $ nextPeer ctx
    stmt                    -> ctx

-- compiles a statement into the SpriL language
compileStmt :: Context -> Statement -> [Instruction]
compileStmt ctx stmt = case stmt of

    -- compiles variable declaration
    GlVarDecl (name, _) expr   -> maybe [] (updateGlVar ctx name) expr
    VarDecl (name, _) expr     -> maybe [] (updateVar ctx name) expr
    
    -- compiles variable assignment
    VarAssign name expr -> 
        if    Map.member name (glVars ctx)
        then  updateGlVar ctx name expr
        else  updateVar ctx name expr

    -- compile parallel execution
    Parallel num script 
        -> startWorkers ctx workers
        ++ skipIfMain ++ body
        ++ joinWorkers ctx workers
        where
            (workers, wrkCtx) = occupyWorkers ctx $ fromInteger num
            skipIfMain = [Jump (Rel $ length body + 1)]
            body = childScope wrkCtx script
                ++ [Jump $ Abs 3] -- jump to busy wait

    -- compiles array insertion
    ArrInsert name idx expr -> updateVarAtIdx ctx name idx expr

    -- puts instructions into a new scope
    InScope script -> childScope ctx script

    -- executes expression without saving the result
    Action expr ->
        let (reg2, ctx2) = occupyReg ctx 
        in  compileExpr ctx2 expr reg2 

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
        ++ prepReturn ctx
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
            elseBody  = peerScope ctx elseScript

    stmt -> error $ "no support for: " ++ show stmt

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

-- puts a script in the peer context
peerScope :: Context -> Script -> [Instruction]
peerScope ctx = putInScope ctx . compileScript (peerCtx ctx)

-----------------------------------------------------------------------------
-- expression compilation
-----------------------------------------------------------------------------

-- compiles an expression into the SpriL language
compileExpr :: Context -> Expr -> RegAddr -> [Instruction]
compileExpr ctx expr reg = case expr of

    Fixed val -> [loadImm (intVal val) reg]

    Var name -> case Map.lookup name (glVars ctx) of
        Just (addr, size)  -> [ReadInstr (DirAddr addr), Receive reg]
        Nothing            -> loadVar ctx name reg

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

    -- FunCall "lock" [Var name] 
    --     -> [TestAnd]

    -- FunCall "unlock" [Var name] 
    --     -> compileExpr ctx expr reg

    FunCall name args
        -- eval arguments and put them on the stack
        -> evalArgs ctx reg args
        -- get function's address
        ++ loadVar ctx ("_f_" ++ name) reg
        -- get function's ARP based on its depth
        ++ loadArp ctx2 (depth - funDepth) reg2
        ++ putVar ctx2 "_arp" reg2
        ++ [copyReg regArp reg2]
        -- set function's context (incl. ARP)
        ++ setNextArp ctx2
        -- save caller's ARP 
        ++ putVar funCtx "_link" reg2
        -- save caller's arguments from the stack
        ++ saveArgs funCtx (reverse argNames)
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

-- compiles a binary operation
compileBin :: Context -> Expr -> Expr -> RegAddr -> Operator -> [Instruction]
compileBin ctx e1 e2 reg op = let reg2 = findReg ctx
    in compileExpr ctx e1 reg
    ++ [Push reg]
    ++ compileExpr ctx e2 reg
    ++ [Pop reg2]
    ++ [Compute op reg2 reg reg]

-- evaluates function arguments and puts them onto the stack
evalArgs :: Context -> RegAddr -> [Expr] -> [Instruction]
evalArgs ctx reg [] = []
evalArgs ctx reg (expr:xs) = 
    compileExpr ctx expr reg 
    ++ [Push reg] 
    ++ evalArgs ctx reg xs

-- saves function arguments in memory
saveArgs :: Context -> [VarName] -> [Instruction]
saveArgs _ [] = []
saveArgs ctx (name:xs) = [Pop reg2] 
    ++ putVar ctx2 name reg2 
    ++ saveArgs ctx xs
    where (reg2, ctx2) = occupyReg ctx

-----------------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------------------

-- translates parsed values to integers
intVal :: Parser.Value -> Integer
intVal (Int val)    = val
intVal (Bool val)   = intBool val
intVal (Char val)   = toInteger $ ord val
intVal val = error $ "failed translating value: " ++ show val

-- updates a variable in shared memory
updateGlVar :: Context -> VarName -> Expr -> [Instruction] 
updateGlVar ctx name expr = 
    compileExpr ctx2 expr reg2
    ++ [WriteInstr reg2 (DirAddr addr)]
    where 
        (addr, size) = fromJust $ Map.lookup name $ glVars ctx 
        (reg2, ctx2) = occupyReg ctx

-- updates a variable in memory at the given index
updateVarAtIdx :: Context -> VarName -> Integer -> Expr -> [Instruction]
updateVarAtIdx ctx name idx expr = case expr of
    Fixed (Arr vals)    -> putArrImm ctx name (intVal <$> vals) 
    expr                -> exprToReg ++ varToMem
    where 
        (reg2, ctx2)    = occupyReg ctx
        exprToReg       = compileExpr ctx2 expr reg2
        varToMem        = putVarAtIdx ctx2 name reg2 idx

-- updates a variable in memory
updateVar :: Context -> VarName -> Expr -> [Instruction]
updateVar ctx name expr = updateVarAtIdx ctx name 0 expr

-- compiles a "skip" condition from an expression
skipCond :: Context -> Expr -> [Instruction] -> [Instruction] 
skipCond ctx expr body = let (reg2, ctx2) = occupyReg ctx in
       compileExpr ctx2 expr reg2
    ++ [Compute Equal reg2 reg0 reg2]
    ++ branchOver reg2 body