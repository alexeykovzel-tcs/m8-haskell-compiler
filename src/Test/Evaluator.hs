module Test.Evaluator where

import Text.ParserCombinators.Parsec
import Parser
import Data.Either
import Syntax

-- Parser

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
            | otherwise  = fromRight' res
  where res = parse p "" xs


-- Evaluator

data Context = Context {
    fun :: FunDef,
    var :: Integer
}

eval :: String -> Integer -> Integer
eval code arg = evalFun (parser parseFun code) arg

evalCond :: Context -> Cond -> Bool
evalCond ctx (Eq x y) = (evalExpr ctx x) == (evalExpr ctx y)

evalExpr :: Context -> Expr -> Integer
evalExpr ctx (Const x)      = x
evalExpr ctx (Var x)        = var ctx
evalExpr ctx (Mult x y)     = (evalExpr ctx x) * (evalExpr ctx y)
evalExpr ctx (Add x y)      = (evalExpr ctx x) + (evalExpr ctx y)
evalExpr ctx (Dec x)        = (evalExpr ctx x) - 1
evalExpr ctx (FunCall _ x)  = evalFun (fun ctx) (evalExpr ctx x)
evalExpr ctx (If cond x y)  = if (evalCond ctx cond) 
                            then (evalExpr ctx x) 
                            else (evalExpr ctx y)

evalFun :: FunDef -> Integer -> Integer
evalFun fun@(Function _ _ expr) var = 
    evalExpr (Context fun var) expr