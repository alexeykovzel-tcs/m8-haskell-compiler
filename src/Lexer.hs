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
        "let", "for", "while", "if", "struct", "fun"
    ],
    Token.commentStart    = "/*",
    Token.commentEnd      = "*/",
    Token.commentLine     = "//",
    Token.identStart      = letter,
    Token.identLetter     = alphaNum
}

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer