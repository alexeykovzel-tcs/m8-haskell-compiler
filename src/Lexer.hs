module Lexer where

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec

languageDef = emptyDef {
    Token.reservedOpNames = [
        "&&", "||",                         -- Logicands
        "==", ">=", "<=", ">", "<",         -- Comparands
        "+", "-",                           -- Terms
        "*", "/", "%"                       -- Factors
    ],
    Token.reservedNames   = [ 
        "let", "struct", "fun",             -- Declarations
        "for", "while", "if", "return",     -- Statements 
        "false", "true",                    -- Bool values
        "Int", "String", "Bool"             -- Basic types
    ],
    Token.commentStart    = "/*",
    Token.commentEnd      = "*/",
    Token.commentLine     = "//",
    Token.identStart      = letter,
    Token.identLetter     = alphaNum
}

lexer = Token.makeTokenParser languageDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

name :: Parser String
name = Token.identifier lexer

comma :: Parser String
comma = Token.comma lexer

semi :: Parser String
semi = Token.semi lexer

colon :: Parser String
colon = Token.colon lexer

intValue :: Parser Integer
intValue = Token.integer lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer