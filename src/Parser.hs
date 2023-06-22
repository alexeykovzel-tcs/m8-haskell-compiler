module Parser (
    Script,
    Statement,
    LoopIter,
    Expr,
    DataType,
    Value,
    VarDef,
    ArgsDef,
    tryParse
) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Maybe
import Lexer

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

type Script = [Statement]

data Statement
    = VarDecl       VarDef (Maybe Expr)
    | VarAssign     String Expr
    | ArrInsert     String Integer Expr
    | FunDef        String ArgsDef (Maybe DataType) Script
    | StructDef     String ArgsDef
    | ForLoop       VarDef LoopIter Script
    | WhileLoop     Expr Script
    | Condition     Expr Script (Maybe Script)
    | ReturnVal     Expr
    | Action        Expr
    deriving Show

data LoopIter
    = IterRange     Integer Integer
    | IterArr       [Value]
    | IterVar       String
    deriving Show

script :: Parser Script
script = whiteSpace *> many statement

statement :: Parser Statement
statement = 
        varDecl         -- e.g. let x: Int;
    <|> try varAssign   -- e.g. x = 3 + y;
    <|> try arrInsert   -- e.g. arr[2] = x + 3;
    <|> funDef          -- e.g. fun increment(x: Int) { }
    <|> structDef       -- e.g. for x: Int in 2..10 { }
    <|> forLoop         -- e.g. while x < 3 { print(x); }
    <|> whileLoop       -- e.g. if x < y { } else { }
    <|> condition       -- e.g. struct Person { first_name: String }
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

structDef :: Parser Statement
structDef = StructDef 
    <$  reserved "struct" <*> name
    <*> braces argsDef

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
    = FunCall       String [Expr]
    | StructDecl    String [Expr]
    | Ternary       Expr Expr Expr
    | Lambda        ArgsDef Script
    | Both          Expr Expr
    | OneOf         Expr Expr
    | Eq            Expr Expr
    | MoreOrEq      Expr Expr
    | LessOrEq      Expr Expr
    | More          Expr Expr
    | Less          Expr Expr
    | Add           Expr Expr
    | Sub           Expr Expr
    | Mult          Expr Expr
    | Div           Expr Expr
    | Mod           Expr Expr
    | Var           String
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
    where op = (Eq       <$ reservedOp "==")
           <|> (MoreOrEq <$ reservedOp ">=")
           <|> (LessOrEq <$ reservedOp "<=")
           <|> (More     <$ reservedOp ">")
           <|> (Less     <$ reservedOp "<")

-- comparand == comparand 
comparand :: Parser Expr
comparand = term `chainl1` op
    where op = (Add <$ reservedOp "+")
           <|> (Sub <$ reservedOp "-")

-- term + term - term
term :: Parser Expr
term = factor `chainl1` op
    where op = (Mult <$ reservedOp "*")
           <|> (Div  <$ reservedOp "/")
           <|> (Mod  <$ reservedOp "%")

-- factor * factor / factor
factor :: Parser Expr
factor = Fixed <$> value
    <|> try lambda          -- e.g. (x: Int) -> { ... }
    <|> parens expr         -- e.g. (2 + 3)
    <|> try funCall         -- e.g. print("Hello")
    <|> try structDecl      -- e.g. Person { "John" }
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

structDecl :: Parser Expr
structDecl = StructDecl 
    <$> name
    <*> braces args

-----------------------------------------------------------------------------
-- data type parsers
-----------------------------------------------------------------------------

data DataType
    = StrType
    | BoolType
    | IntType
    | StructType String                 -- struct name
    | ArrType DataType (Maybe Integer)  -- type + size
    deriving Show

dataType :: Parser DataType
dataType = foldl ArrType <$> baseType <*> arrDecl
    where arrDecl = many $ brackets $ nullable integer 

baseType :: Parser DataType
baseType = StrType    <$  reserved "String"
       <|> BoolType   <$  reserved "Bool"
       <|> IntType    <$  reserved "Int"
       <|> StructType <$> name

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
-- other parsers
-----------------------------------------------------------------------------

type VarDef = (String, Maybe DataType)

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