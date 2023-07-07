module Lexer where

{- author: Aliaksei Kouzel - s2648563 -}

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec

-----------------------------------------------------------------------------
-- language definition
-----------------------------------------------------------------------------

languageDef = emptyDef {
    Token.reservedOpNames = [
        "&&", "||",                         -- logicand operators
        "!=", "==", ">=", "<=", ">", "<",   -- comparand operators
        "+", "-",                           -- term operators
        "*", "/", "%",                      -- factor operators
        ":", "?"                            -- ternary operators
    ],
    Token.reservedNames   = [ 
        "let", "global", "for", "while", "if",
        "fun", "return", "parallel",
        "Int", "Char", "Bool", "String",
        "false", "true"
    ],
    Token.commentStart = "/*",
    Token.commentEnd   = "*/",
    Token.commentLine  = "//",
    Token.identStart   = letter,
    Token.identLetter  = alphaNum <|> char '_'
}

-----------------------------------------------------------------------------
-- basic parsers
-----------------------------------------------------------------------------

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

integer :: Parser Integer
integer = Token.integer lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

nullable :: Parser a -> Parser (Maybe a)
nullable p = Just <$> p <|> pure Nothing