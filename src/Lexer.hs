module Lexer where

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec

languageDef = emptyDef {
    Token.reservedOpNames = [
        "&&", "||",
        "==", ">=", "<=", ">", "<",
        "+", "-", "*", "/", "%"
    ],
    Token.reservedNames   = [ 
        "let", "struct", "fun",
        "for", "while", "if",
        "false", "true",
        "Int", "String", "Bool",
        "return"
    ],
    Token.commentStart    = "/*",
    Token.commentEnd      = "*/",
    Token.commentLine     = "//",
    Token.identStart      = letter,
    Token.identLetter     = alphaNum
}

lexer = Token.makeTokenParser languageDef

name :: Parser String
name = Token.identifier lexer

int :: Parser Integer
int = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer