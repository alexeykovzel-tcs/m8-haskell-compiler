module Parser where

import Text.ParserCombinators.Parsec
import Syntax

-- Parsing conditions

parseExpr :: Parser Expr
parseExpr = 
    parseDec 
    <|> parseIf
    <|> ((\a b -> foldl Add a b) 
        <$> parseTerm 
        <*> (many $ symbol "+" *> parseTerm))
    <?> "expression"


parseFactor :: Parser Expr
parseFactor = 
    (Const <$> num) 
    <|> (parens parseExpr)
    <|> try (FunCall 
            <$> identifier 
            <*> (parens parseExpr)) 
    <|> (Var <$> identifier) 
    <?> "id or number"


parseTerm :: Parser Expr
parseTerm = 
    (\a b -> foldl Mult a b) 
    <$> parseFactor 
        <*> (many $ symbol "*" *> parseFactor)
    <?> "term (e.g. 2*3*5)"


-- Parsing conditions

parseCond :: Parser Cond
parseCond = Eq 
    <$> parseExpr <*> (string "==" *> parseExpr)
    <?> "condition"

parseDec :: Parser Expr
parseDec = Dec 
    <$> (symbol "dec" *> parseExpr)
    <?> "dec (wtf is it)"

parseIf :: Parser Expr
parseIf = If 
    <$> (symbol "if" *> parseCond) 
        <*> (symbol "then" *> parseExpr) 
        <*> (symbol "else" *> parseExpr)


-- Parsing functions

parseFun :: Parser FunDef
parseFun = Function 
    <$> (symbol "function" *> identifier)
        <*> identifier
        <*> (symbol "=" *> parseExpr)