module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Maybe
import Lexer
import Test.QuickCheck as QC


-- TODO: Error handling.
-- TODO: More testing.

-----------------------------------------------------------------------------
-- parser usage
-----------------------------------------------------------------------------

-- tryParse statement    "let x: Int = 2 + 1";
-- tryParse statement    "for i: Int in [1, 2, 3] { print(i); }"
-- tryParse expr         "2 * (3 + 4) / 6"

tryParse :: Parser a -> String -> a
tryParse p xs = either (error . show) id $ parse p "" xs

-----------------------------------------------------------------------------
-- statement parsers
-----------------------------------------------------------------------------

type FunName = String
type VarName = String

type Script = [Statement]

data Statement
    = VarDecl       VarDef (Maybe Expr)
    | VarAssign     VarName Expr
    | ArrInsert     VarName Integer Expr
    | FunDef        FunName ArgsDef (Maybe DataType) Script
    | ForLoop       VarDef LoopIter Script
    | WhileLoop     Expr Script
    | Condition     Expr Script (Maybe Script)
    | ReturnVal     Expr
    | Action        Expr
    deriving Show

data LoopIter
    = IterRange     Integer Integer
    | IterArr       [Value]
    | IterVar       VarName
    deriving Show

script :: Parser Script
script = whiteSpace *> many statement

statement :: Parser Statement
statement = 
        varDecl         -- e.g. let x: Int;
    <|> try varAssign   -- e.g. x = 3 + y;
    <|> try arrInsert   -- e.g. arr[2] = x + 3;
    <|> funDef          -- e.g. fun increment(x: Int) -> Int { }
    <|> forLoop         -- e.g. for x: Int in 2..10 { }
    <|> whileLoop       -- e.g. while x < 3 { print(x); }
    <|> condition       -- e.g. if x < y { } else { }
    <|> returnVal       -- e.g. return x;
    <|> action          -- e.g. print(x);

varDecl :: Parser Statement
varDecl = VarDecl
    <$  reserved "let" <*> varDef
    <*> nullable (symbol "=" *> expr)
    <*  semi

varAssign :: Parser Statement
varAssign = VarAssign 
    <$> name
    <*  symbol "=" <*> expr 
    <*  semi

arrInsert :: Parser Statement
arrInsert = ArrInsert 
    <$> name
    <*> brackets integer
    <*  symbol "=" <*> expr 
    <*  semi

funDef :: Parser Statement
funDef = FunDef
    <$  reserved "fun" <*> name
    <*> parens argsDef 
    <*> nullable (symbol "->" *> dataType)
    <*> braces script

forLoop :: Parser Statement
forLoop = ForLoop 
    <$  reserved "for" <*> varDef
    <*  symbol "in" <*> loopIter
    <*> braces script

loopIter :: Parser LoopIter
loopIter = 
        IterRange <$> integer <* symbol ".." <*> integer
    <|> IterArr   <$> array
    <|> IterVar   <$> name

whileLoop :: Parser Statement
whileLoop = WhileLoop
    <$  reserved "while" <*> expr 
    <*> braces script

condition :: Parser Statement
condition = Condition 
    <$  reserved "if" <*> expr 
    <*> braces script
    <*> nullable (reserved "else" *> braces script)

returnVal :: Parser Statement
returnVal = ReturnVal
    <$  reserved "return" <*> expr
    <*  semi

action :: Parser Statement
action = Action <$> expr <*  semi

-----------------------------------------------------------------------------
-- expression parsers
-----------------------------------------------------------------------------

data Expr
    = FunCall       FunName [Expr]
    | Ternary       Expr Expr Expr
    | Lambda        ArgsDef Script
    | Both          Expr Expr
    | OneOf         Expr Expr
    | Eq            Expr Expr
    | MoreEq        Expr Expr
    | LessEq        Expr Expr
    | More          Expr Expr
    | Less          Expr Expr
    | Add           Expr Expr
    | Sub           Expr Expr
    | Mult          Expr Expr
    | Var           VarName
    | Fixed         Value
    deriving Show

expr :: Parser Expr
expr =  try ternary 
    <|> operation 
    <?> "expression"

ternary :: Parser Expr
ternary = Ternary 
    <$> operation 
    <*  symbol "?" <*> operation
    <*  symbol ":" <*> operation

operation :: Parser Expr
operation = logicand `chainl1` op
    where op = (Both  <$ reservedOp "&&") 
           <|> (OneOf <$ reservedOp "||")

-- logicand && logicand || logicand 
logicand :: Parser Expr
logicand = comparand `chainl1` op
    where op = (Eq      <$ reservedOp "==")
           <|> (MoreEq  <$ reservedOp ">=")
           <|> (LessEq  <$ reservedOp "<=")
           <|> (More    <$ reservedOp ">")
           <|> (Less    <$ reservedOp "<")

-- comparand == comparand 
comparand :: Parser Expr
comparand = term `chainl1` op
    where op = (Add <$ reservedOp "+")
           <|> (Sub <$ reservedOp "-")

-- term + term - term
term :: Parser Expr
term = factor `chainl1` op
    where op = (Mult <$ reservedOp "*")

-- factor * factor / factor
factor :: Parser Expr
factor = Parser.Fixed <$> value
    <|> try lambda          -- e.g. (x: Int) -> { ... }
    <|> parens expr         -- e.g. (2 + 3)
    <|> try funCall         -- e.g. print("Hello")
    <|> Var <$> name

lambda :: Parser Expr
lambda = Lambda 
    <$> parens argsDef 
    <*  symbol "->" 
    <*> braces script

funCall :: Parser Expr
funCall = FunCall 
    <$> name 
    <*> parens args

-----------------------------------------------------------------------------
-- data type parsers
-----------------------------------------------------------------------------

type ArrSize = Integer

data DataType
    = StrType
    | BoolType
    | IntType
    | ArrType DataType (Maybe ArrSize)
    deriving Show

dataType :: Parser DataType
dataType = foldl ArrType <$> baseType <*> arrDecl
    where arrDecl = many $ brackets $ nullable integer 

baseType :: Parser DataType
baseType = StrType    <$  reserved "String"
       <|> BoolType   <$  reserved "Bool"
       <|> IntType    <$  reserved "Int"

-----------------------------------------------------------------------------
-- value parsers
-----------------------------------------------------------------------------

data Value
    = Text   String 
    | Bool   Bool 
    | Int    Integer
    | Arr    [Value]
    | None
    deriving (Show, Eq)

value :: Parser Value
value = Int  <$> integer
    <|> Text <$> text
    <|> Arr  <$> array
    <|> Bool <$> boolean
    <|> None <$  reserved "None"

text :: Parser String
text = between (char '"') (char '"') (many textChar)
    where 
        textChar = escapeChar <|> noneOf "\"\\"
        escapeChar = char '\\' *> oneOf "\"\\"

array :: Parser [Value]
array = brackets $ value `sepBy` comma

boolean :: Parser Bool
boolean = False <$ reserved "false"
      <|> True  <$ reserved "true"

-----------------------------------------------------------------------------
-- Generator for script
-----------------------------------------------------------------------------

instance QC.Arbitrary Statement where
    arbitrary = QC.oneof [
        VarDecl     <$> QC.arbitrary <*> QC.arbitrary,
        VarAssign   <$> QC.arbitrary <*> QC.arbitrary,
        ArrInsert   <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        FunDef      <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        ForLoop     <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        WhileLoop   <$> QC.arbitrary <*> QC.arbitrary,
        Condition   <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
        ReturnVal   <$> QC.arbitrary,
        Action      <$> QC.arbitrary ]

instance QC.Arbitrary Expr where
    arbitrary = QC.sized expr
        where
            expr 0 = Parser.Fixed <$> QC.arbitrary
            expr n = let nextExpr = expr (n `div` 2) in QC.oneof [
                FunCall <$> QC.arbitrary <*> QC.resize (n `div` 2) QC.arbitrary,
                Lambda  <$> QC.arbitrary <*> QC.arbitrary,
                Ternary <$> nextExpr <*> nextExpr <*> nextExpr,
                Both    <$> nextExpr <*> nextExpr,
                OneOf   <$> nextExpr <*> nextExpr,
                Eq      <$> nextExpr <*> nextExpr,
                MoreEq  <$> nextExpr <*> nextExpr,
                LessEq  <$> nextExpr <*> nextExpr,
                More    <$> nextExpr <*> nextExpr,
                Less    <$> nextExpr <*> nextExpr,
                Add     <$> nextExpr <*> nextExpr,
                Sub     <$> nextExpr <*> nextExpr,
                Mult    <$> nextExpr <*> nextExpr,
                Var     <$> QC.arbitrary,
                Parser.Fixed <$> QC.arbitrary ]

instance QC.Arbitrary DataType where
    arbitrary = QC.oneof [
        pure StrType,
        pure BoolType,
        pure IntType,
        ArrType <$> QC.arbitrary <*> QC.arbitrary ]

instance QC.Arbitrary Value where
    arbitrary = QC.oneof [
        Text <$> QC.arbitrary,
        Bool <$> QC.arbitrary,
        Int  <$> QC.arbitrary,
        Arr  <$> QC.arbitrary,
        pure None ]

instance QC.Arbitrary LoopIter where
    arbitrary = QC.oneof [
        IterRange <$> QC.arbitrary <*> QC.arbitrary,
        IterArr   <$> QC.listOf QC.arbitrary,
        IterVar   <$> QC.arbitrary ]

autoScript :: IO Script
autoScript = QC.generate (QC.resize 3 QC.arbitrary)

-----------------------------------------------------------------------------
-- other parsers
-----------------------------------------------------------------------------

type VarDef = (VarName, Maybe DataType)

type ArgsDef = [VarDef]

varDef :: Parser VarDef
varDef = (,) <$> name <*> typeDecl
    where typeDecl = nullable $ colon *> dataType

argsDef :: Parser ArgsDef
argsDef = varDef `sepBy` comma

args :: Parser [Expr]
args = expr `sepBy` comma

nullable :: Parser a -> Parser (Maybe a)
nullable p = Just <$> p <|> pure Nothing