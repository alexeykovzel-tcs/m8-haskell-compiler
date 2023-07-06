{-# LANGUAGE FlexibleInstances #-}

module Compiler (compile) where

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
compile code 
    | threadCount == 0 =  initArp ++ progASM ++ [EndProg]
    | otherwise     = initArp ++ (Branch regSprID)
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

    InScope script  -> simpleScope ctx script
    Action expr     -> voidExpr ctx expr

    -- FunDef name argsDef returnType script
    --     -> putInScope ctx $ fun
    --     where
    --         fun    = args ++ body
    --         body   = compileScript ctxScp script
    --         args   = []
    --         ctxScp = inScopeCtx ctx
 
    -- ReturnVal expr -> 

    WhileLoop expr script -> cond ++ body
        where
            cond      = skipCond ctx expr body
            body      = simpleScope ctx script ++ jumpBack [body, cond]

    Condition expr ifScript Nothing -> cond ++ ifBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = simpleScope ctx ifScript

    Condition expr ifScript (Just elseScript) 
        -> cond ++ ifBody ++ elseBody
        where
            cond      = skipCond ctx expr ifBody
            ifBody    = simpleScope ctx ifScript ++ jumpOver elseBody
            elseBody  = elseScope ctx elseScript
    -- Fork name script -> compileFork ctx name script
    -- Join name -> compileJoin x ctx

-- sets context scope for a script
simpleScope :: Context -> Script -> [Instruction]
simpleScope ctx = putInScope ctx . compileScript (inScopeCtx ctx)

-- sets context scope for the "else" script
elseScope :: Context -> Script -> [Instruction]
elseScope ctx = putInScope ctx . compileScript (inScopeCtxElse ctx)

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
 
    FunCall "print_str" [Fixed msg]
        -> printStrLn ctx (show msg)

    FunCall "print_str" [Var name]
        -> applyArr ctx name (\reg -> [WriteInstr reg charIO])
        ++ printChar reg '\n'

    FunCall "print" [expr]
        -> compileExpr ctx expr reg 
        ++ [WriteInstr reg numberIO] 

    -- Update ARP 
    -- Load return address
    -- Load arguments
    -- Jump to codeAddr
    FunCall name args
        ->  [
                -- loadImm codeAddr regArp,
                -- addImm ctx2 regPC reg2
                -- putVar ctx2 "_ra" reg2      -- set return address
                -- Jump $ Abs codeAddr
            ]
        where 
            (scopeId, codeAddr)    = fromJust $ Map.lookup name (funMap ctx)
            (varMap, depth, size)  = fromJust $ Map.lookup scopeId (scopeMap ctx)
            (reg2, ctx2)           = occupyReg ctx

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


-- Count the maximum number of concurrent threads
maxConcurrentThreads :: Script -> Int
maxConcurrentThreads = count 0 0
  where
    count maxThreads currentThreads [] = maxThreads
    count maxThreads currentThreads (stmt:stmts)
      | isFork stmt = count (max maxThreads (currentThreads + 1)) (currentThreads + 1) stmts
      | isJoin stmt = count maxThreads (currentThreads - 1) stmts
      | otherwise   = count maxThreads currentThreads stmts

    isFork (Fork _ _) = True
    isFork _          = False
    isJoin (Join _)   = True
    isJoin _          = False