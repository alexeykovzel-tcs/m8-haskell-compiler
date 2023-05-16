module Syntax where

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

languageDef = emptyDef {
    Token.reservedOpNames = ["+", "*"]
}

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
parens = Token.parens lexer
reserved = Token.reserved lexer
symbol = Token.symbol lexer
num = Token.integer lexer

data Expr 
    = Const Integer
    | Var String
    | Mult Expr Expr
    | Add Expr Expr
    | If Cond Expr Expr
    | FunCall String Expr
    | Dec Expr 
    deriving Show

data Cond 
    = Eq Expr Expr
    deriving Show

data FunDef 
    = Function String String Expr
    deriving Show