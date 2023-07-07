{-# LANGUAGE FlexibleInstances #-}

{- author: Aliaksei Kouzel - s2648563 -}

module Compiler (
    compile,
    compileFile,
    compileFunCall
) where

import Sprockell
import SprockellExt
import PreCompiler
import Elaborator (elaborate)
import Parser
import Data.Maybe
import Data.Char (ord)
import qualified Data.Map as Map

-- compiles a program from a string
compile :: String -> [[Instruction]]
compile code = replicate num $ prog
    where
        ast   = elaborate $ parseScript code
        num   = fromInteger $ countWorkers ast + 1
        prog  = compileAST num ast

-- compiles a program from a file
compileFile :: FilePath -> IO [[Instruction]]
compileFile file = compile <$> readFile file

-- compiles a program from a string with a function call at the end
compileFunCall :: String -> FunName -> [Integer] -> [Instruction]
compileFunCall code funName args = compileAST 0 $ ast ++ [Action funCall]
    where 
        ast      = elaborate $ parseScript code
        funCall  = FunCall "print" [FunCall funName funArgs]
        funArgs  = Fixed <$> Int <$> args

-- compiles a program from the parser result (AST)
compileAST :: Int -> Script -> [Instruction]
compileAST num ast = startProg ++ prog ++ endProg ctx
    where
        script   = postScript ast
        ctx      = initCtx script num
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

    -- updates a variable value
    ArrInsert name idx expr     -> updateLcVarAtIdx ctx name idx expr
    GlVarDecl (name, _) expr    -> maybe [] (updateGlVar ctx name) expr
    VarDecl (name, _) expr      -> maybe [] (updateLcVar ctx name) expr
    VarAssign name expr         -> 

        if    Map.member name (glVars ctx)
        then  updateGlVar ctx name expr
        else  updateLcVar ctx name expr

    -- parallelly executes a script in {num} processes
    Parallel num script 
        -> startWorkers ctx workers
        ++ skipIfMain 
        ++ body
        ++ joinWorkers ctx workers
        where
            (workers, wrkCtx) = occupyWorkers ctx $ fromInteger num
            skipIfMain = [Jump $ Rel $ length body + 1]
            body = childScope wrkCtx script ++ [Jump $ Abs 3]

    -- executes an expression without saving the result
    Action expr ->
        let (reg2, ctx2) = occupyReg ctx 
        in  compileExpr ctx2 expr reg2 

    -- compiles but not executes a function
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

    -- calculates a script while the condition holds
    WhileLoop expr script -> cond ++ body
        where
            cond      = skipCond ctx expr body
            body      = childScope ctx script ++ jumpBack [body, cond]

    -- calculates an if condition
    Condition expr ifScript Nothing -> cond ++ ifBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = childScope ctx ifScript

    -- calculates an if/else condition
    Condition expr ifScript (Just elseScript) 
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = childScope ctx ifScript ++ jumpOver elseBody
            elseBody  = peerScope ctx elseScript

    -- puts a script into the next scope
    InScope script -> childScope ctx script

    stmt -> error $ "no support for: " ++ show stmt

-- prepares for a function return
prepReturn :: Context -> [Instruction]
prepReturn ctx = returnAddr ++ callerArp ++ [Jump $ Ind reg2]
    where 
        (reg2, ctx2)  = occupyReg ctx
        returnAddr    = loadVar ctx2 "_rtn_addr" reg2
        callerArp     = loadVar ctx2 "_link" regArp

-- puts a script in the next scope on the next depth
childScope :: Context -> Script -> [Instruction]
childScope ctx = putInScope ctx . compileScript (childCtx ctx)

-- puts a script in the next scope on the same depth 
peerScope :: Context -> Script -> [Instruction]
peerScope ctx = putInScope ctx . compileScript (peerCtx ctx)

-----------------------------------------------------------------------------
-- expression compilation
-----------------------------------------------------------------------------

-- compiles an expression into the SpriL language
compileExpr :: Context -> Expr -> RegAddr -> [Instruction]
compileExpr ctx expr reg = case expr of

    -- calculates a binary operation
    Parser.Add  e1 e2 -> compileBin ctx e1 e2 reg Sprockell.Add
    Parser.Sub  e1 e2 -> compileBin ctx e1 e2 reg Sprockell.Sub
    Mult        e1 e2 -> compileBin ctx e1 e2 reg Mul
    Eq          e1 e2 -> compileBin ctx e1 e2 reg Equal
    NotEq       e1 e2 -> compileBin ctx e1 e2 reg NEq
    MoreEq      e1 e2 -> compileBin ctx e1 e2 reg GtE
    LessEq      e1 e2 -> compileBin ctx e1 e2 reg LtE
    More        e1 e2 -> compileBin ctx e1 e2 reg Gt
    Less        e1 e2 -> compileBin ctx e1 e2 reg Lt
    Both        e1 e2 -> compileBin ctx e1 e2 reg And
    OneOf       e1 e2 -> compileBin ctx e1 e2 reg Or

    -- calculates a ternary operator
    Ternary expr1 expr2 expr3 -> cond ++ ifBody ++ elseBody
        where
            cond       = skipCond ctx expr1 ifBody
            ifBody     = compileExpr ctx expr2 reg ++ jumpOver elseBody
            elseBody   = compileExpr ctx expr3 reg

    -- calculates an expression and reverts its sign
    Neg expr -> compileExpr ctx expr reg ++ negateReg reg 

    -- value loaders
    ArrAccess name idx   -> loadVarAtIdx ctx name reg idx
    Fixed val            -> [loadImm (intVal val) reg]
    Var name             -> loadVar ctx name reg

    -- stores the current process ID
    FunCall "set_process_id" [Var name] -> putVar ctx name regSprID
 
    -- print functions
    FunCall "print_str" [Fixed msg]  -> printStrLn ctx (show msg)
    FunCall "print_str" [Var name]   -> printVarStr ctx name
    FunCall "print" [expr]           -> compileExpr ctx expr reg 
                                        ++ printReg reg 
    -- synchronizers
    FunCall "lock" [Var name]        -> lockMem reg $ glVarAddr ctx name
    FunCall "unlock" [Var name]      -> unlockMem $ glVarAddr ctx name

    FunCall "error" [Fixed msg]      -> printStrLn ctx ("error: " ++ show msg)
                                        ++ [EndProg]

    -- ultimate function call
    FunCall name args
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
        -- save caller's arguments from stack
        ++ saveArgs funCtx (reverse argNames)
        -- save return address
        ++ putPC funCtx "_rtn_addr"
        ++ [Jump $ Ind reg]
        -- retrieve return value
        ++ loadAI ctx regArp size reg
        where 
            funCtx            = ctx2 { scopeId = scopeId }
            (reg2, ctx2)      = occupyReg ctx
            (_, depth, size)  = getScope ctx
            (scopeId, funDepth, argNames) 
                = fromJust $ Map.lookup name $ funMap ctx

-- compiles a binary operation and puts the result to a register
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

-- saves function arguments from stack to memory
saveArgs :: Context -> [VarName] -> [Instruction]
saveArgs _ [] = []
saveArgs ctx (name:xs) = [Pop reg2] 
    ++ putVar ctx2 name reg2 
    ++ saveArgs ctx xs
    where (reg2, ctx2) = occupyReg ctx

-----------------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------------------

-- translates values from the parser to integers
intVal :: Parser.Value -> Integer
intVal (Int val)    = val
intVal (Bool val)   = intBool val
intVal (Char val)   = toInteger $ ord val
intVal val = error $ "failed translating value: " ++ show val

-- updates a variable in the shared memory
updateGlVar :: Context -> VarName -> Expr -> [Instruction] 
updateGlVar ctx name expr = case expr of
    -- Fixed (Arr vals)    -> putGlArr addr $ intVal <$> vals
    expr                -> compileExpr ctx2 expr reg2
                        ++ [WriteInstr reg2 (DirAddr addr)]
    where 
        (reg2, ctx2) = occupyReg ctx
        (addr, size) = fromJust $ Map.lookup name $ glVars ctx 

-- updates a local variable in memory at the given index
updateLcVarAtIdx :: Context -> VarName -> Integer -> Expr -> [Instruction]
updateLcVarAtIdx ctx name idx expr = case expr of
    Fixed (Arr vals)    -> putArrImm ctx name (intVal <$> vals) 
    expr                -> exprToReg ++ varToMem
    where 
        (reg2, ctx2)    = occupyReg ctx
        exprToReg       = compileExpr ctx2 expr reg2
        varToMem        = putVarAtIdx ctx2 name reg2 idx

-- updates a local variable in memory
updateLcVar :: Context -> VarName -> Expr -> [Instruction]
updateLcVar ctx name expr = updateLcVarAtIdx ctx name 0 expr

-- compiles a "skip" condition from an expression
skipCond :: Context -> Expr -> [Instruction] -> [Instruction] 
skipCond ctx expr body = let (reg2, ctx2) = occupyReg ctx in
       compileExpr ctx2 expr reg2
    ++ [Compute Equal reg2 reg0 reg2,
        branchOver reg2 body]