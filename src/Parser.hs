module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Maybe
import Lexer

{-
    -- Usage examples --

    parser statement    "let x: Int = 2 + 1";
    parser statement    "for i: Int in [1, 2, 3] { print(i); }"
    parser expr         "2 * (3 + 4) / 6"
-}

parser :: Parser a -> String -> a
parser p xs = either (error . show) id result
  where result = parse p "" xs


{------------------------}
{-        @SCRIPT       -}
{------------------------}

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
    | IterArr       [Val]
    | IterVar       String
    deriving Show

script :: Parser Script
script = whiteSpace *> many statement

statement :: Parser Statement
statement 
    =   varDecl             -- e.g. let x: Int;
    <|> try varAssign     -- e.g. x = 3 + y;
    <|> try arrInsert     -- e.g. arr[2] = x + 3;
    <|> funDef              -- e.g. fun increment(x: Int) { ... }
    <|> structDef           -- e.g. for x: Int in 2..10 { ... }
    <|> forLoop             -- e.g. while x < 3 { print(x); }
    <|> whileLoop           -- e.g. if x < y { ... } else { ... }
    <|> condition           -- e.g. struct Person { first_name: String }
    <|> returnVal           -- e.g. return x;
    <|> action              -- e.g. print(x);

varDecl :: Parser Statement
varDecl = VarDecl
    <$  reserved "let" <*> varDef
    <*> (Just 
        <$> (symbol "=" *> expr) 
        <|> pure Nothing)
    <*  semi

varAssign :: Parser Statement
varAssign = VarAssign 
    <$> name
    <*  symbol "=" <*> expr 
    <*  semi

arrInsert :: Parser Statement
arrInsert = ArrInsert 
    <$> name
    <*> brackets intVal
    <*  symbol "=" <*> expr 
    <*  semi

funDef :: Parser Statement
funDef = FunDef
    <$  reserved "fun" <*> name
    <*> parens argsDef 
    <*> (Just
        <$> (symbol "->" *> dataType)
        <|> pure Nothing)
    <*> braces script

forLoop :: Parser Statement
forLoop = ForLoop 
    <$  reserved "for" <*> varDef
    <*  symbol "in" <*> loopIter
    <*> braces script

loopIter :: Parser LoopIter
loopIter
    =   IterRange   <$> intVal <* symbol ".." <*> intVal
    <|> IterArr     <$> arrVal
    <|> IterVar     <$> name

whileLoop :: Parser Statement
whileLoop = WhileLoop
    <$  reserved "while" <*> expr 
    <*> braces script

condition :: Parser Statement
condition = Condition 
    <$  reserved "if" <*> expr 
    <*> braces script
    <*> (Just 
        <$> (reserved "else" *> braces script)
        <|> pure Nothing)

structDef :: Parser Statement
structDef = StructDef 
    <$  reserved "struct" <*> name
    <*> braces argsDef

returnVal :: Parser Statement
returnVal = ReturnVal
    <$  reserved "return" <*> expr
    <*  semi

action :: Parser Statement
action = Action 
    <$> expr 
    <*  semi


{------------------------}
{-     @EXPRESSIONS     -}
{------------------------}

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
    | Fixed         Val
    deriving Show

expr :: Parser Expr
expr = try ternary <|> operation

ternary :: Parser Expr
ternary = Ternary 
    <$> operation 
    <*  symbol "?" <*> operation 
    <*  symbol ":" <*> operation

operation :: Parser Expr
operation = logicand `chainl1` op
    where op 
            =   (Both       <$ reservedOp "&&") 
            <|> (OneOf      <$ reservedOp "||")

-- logicand && logicand || logicand 
logicand :: Parser Expr
logicand = comparand `chainl1` op
    where op 
            =   (Eq         <$ reservedOp "==")
            <|> (MoreOrEq   <$ reservedOp ">=")
            <|> (LessOrEq   <$ reservedOp "<=")
            <|> (More       <$ reservedOp ">")
            <|> (Less       <$ reservedOp "<")

-- comparand == comparand 
comparand :: Parser Expr
comparand = term `chainl1` op
    where op 
            =   (Add        <$ reservedOp "+")
            <|> (Sub        <$ reservedOp "-")

-- term + term - term
term :: Parser Expr
term = factor `chainl1` op
    where op 
            =   (Mult       <$ reservedOp "*")
            <|> (Div        <$ reservedOp "/")
            <|> (Mod        <$ reservedOp "%")

-- factor * factor / factor
factor :: Parser Expr
factor 
    =   Fixed <$> val
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
    <$> name <*> parens args

structDecl :: Parser Expr
structDecl = StructDecl 
    <$> name
    <*> braces args


{------------------------}
{-         @ARGS        -}
{------------------------}

type ArgsDef = [VarDef]

argsDef :: Parser ArgsDef
argsDef = varDef `sepBy` comma

args :: Parser [Expr]
args = expr `sepBy` comma


{------------------------}
{-        @VALUES       -}
{------------------------}

data VarDef 
    = VarDef String (Maybe DataType)
    deriving Show

data Val
    = Str String 
    | Bool Bool 
    | Int Integer
    | Arr [Val]
    | None
    deriving (Show, Eq)

data DataType
    = StrType
    | BoolType
    | IntType
    | StructType
    | ArrType DataType (Maybe Integer)
    deriving Show

baseType :: Parser DataType
baseType 
    =   StrType     <$  reserved "String"
    <|> BoolType    <$  reserved "Bool"
    <|> IntType     <$  reserved "Int"
    <|> StructType  <$  name

arrSpec :: Parser [Maybe Integer]
arrSpec = many $ brackets 
    (Just <$> intVal <|> pure Nothing)

dataType :: Parser DataType
dataType = foldl ArrType 
    <$> baseType 
    <*> arrSpec

varDef :: Parser VarDef
varDef = VarDef 
    <$> name
    <*> (Just
        <$> (colon *> dataType)
        <|> pure Nothing)

val :: Parser Val
val 
    =   Int     <$> intVal
    <|> Str     <$> strVal
    <|> Arr     <$> arrVal
    <|> Bool    <$> boolVal
    <|> None    <$  reserved "None"

strVal :: Parser String
strVal = between (char '"') (char '"') (many strChar)
    where 
        strChar = escapeChar <|> noneOf "\"\\"
        escapeChar = char '\\' *> oneOf "\"\\"

arrVal :: Parser [Val]
arrVal = brackets $ val `sepBy` comma

boolVal :: Parser Bool
boolVal
    =   False       <$ reserved "false"
    <|> True        <$ reserved "true"

-- nameUse :: Parser NameUse
-- nameUse = (,) <$> name <*> getPosition