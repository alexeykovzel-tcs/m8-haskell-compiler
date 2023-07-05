module Parser where

import Test.QuickCheck as QC
import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Lexer

-----------------------------------------------------------------------------
-- parser usage
-----------------------------------------------------------------------------

-- parseWith script    "let x: Int = 2 + 1";
-- parseWith script    "for i: Int in [1, 2, 3] { print(i); }"
-- parseWith expr      "2 * (3 + 4) / 6"

-- parses a string with the given parser
parseWith :: Parser a -> String -> a
parseWith p xs = either (error . show) id $ parse p "" xs

-----------------------------------------------------------------------------
-- statement parsing
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
    | InScope       Script
    | ReturnVal     Expr
    | Action        Expr
    deriving Show

data LoopIter 
    = IterRange Expr Expr
    deriving Show

script :: Parser Script
script = whiteSpace *> many statement

statement :: Parser Statement
statement =   
        varDecl         -- e.g. let x: Int;
    <|> try varAssign   -- e.g. x = 3 + y;
    <|> try arrInsert   -- e.g. arr[2] = x + 3;
    <|> inScope         -- e.g. { ... }
    <|> funDef          -- e.g. fun incr(x: Int) -> Int { ... }
    <|> forLoop         -- e.g. for x: Int in 2..10 { ... }
    <|> whileLoop       -- e.g. while x < 3 { ... }
    <|> condition       -- e.g. if x < y { ... } else { ... }
    <|> returnVal       -- e.g. return x;
    <|> action          -- e.g. print(x);

-- parses a variable declaration
varDecl :: Parser Statement
varDecl = VarDecl
    <$  reserved "let" <*> varDef
    <*> nullable (symbol "=" *> expr)
    <*  semi

-- parses a variable assignment
varAssign :: Parser Statement
varAssign = VarAssign 
    <$> name
    <*  symbol "=" <*> expr 
    <*  semi

-- parses an array insertion
arrInsert :: Parser Statement
arrInsert = ArrInsert 
    <$> name
    <*> brackets integer
    <*  symbol "=" <*> expr 
    <*  semi

-- parses a typical scope
inScope :: Parser Statement
inScope = InScope 
    <$> braces script

-- parses a function definition
funDef :: Parser Statement
funDef = FunDef
    <$  reserved "fun" <*> name
    <*> parens argsDef 
    <*> nullable (symbol "->" *> dataType)
    <*> braces script

-- parses a "for" loop
forLoop :: Parser Statement
forLoop = ForLoop 
    <$  reserved "for" <*> varDef
    <*  symbol "in" <*> loopIter
    <*> braces script

-- parses a loop iterator
loopIter :: Parser LoopIter
loopIter = IterRange 
    <$> expr <* symbol ".." <*> expr

-- parses a "while" loop
whileLoop :: Parser Statement
whileLoop = WhileLoop
    <$  reserved "while" <*> expr 
    <*> braces script

-- parses an if/else condition
condition :: Parser Statement
condition = Condition 
    <$  reserved "if" <*> expr 
    <*> braces script
    <*> nullable (reserved "else" *> braces script)

-- parses a return value
returnVal :: Parser Statement
returnVal = ReturnVal
    <$  reserved "return" <*> expr
    <*  semi

-- parses an expression, which result is ignored
action :: Parser Statement
action = Action 
    <$> expr <* semi

-----------------------------------------------------------------------------
-- expression parsing
-----------------------------------------------------------------------------

data Expr
    = FunCall       FunName [Expr]
    | Ternary       Expr Expr Expr
    | ArrAccess     VarName Integer
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
    | Neg           Expr
    | Var           VarName
    | Fixed         Value
    deriving Show

-- parses an expression
expr :: Parser Expr
expr =  try ternary 
    <|> operation
    <?> "expression"

-- parses a ternary operator
ternary :: Parser Expr
ternary = Ternary 
    <$> operation 
    <*  reservedOp "?" <*> operation
    <*  reservedOp ":" <*> operation

-- parses an operation that returns a value
operation :: Parser Expr
operation = logicand `chainl1` op
    where op = (Both  <$ reservedOp "&&") 
           <|> (OneOf <$ reservedOp "||")

-- logicands are chained like: a && b || c 
logicand :: Parser Expr
logicand = comparand `chainl1` op
    where op = (Eq      <$ reservedOp "==")
           <|> (MoreEq  <$ reservedOp ">=")
           <|> (LessEq  <$ reservedOp "<=")
           <|> (More    <$ reservedOp ">")
           <|> (Less    <$ reservedOp "<")

-- comparands are chained like: a == b 
comparand :: Parser Expr
comparand = term `chainl1` op
    where op = (Add <$ reservedOp "+")
           <|> (Sub <$ reservedOp "-")

-- terms are chained like: a + b - c
term :: Parser Expr
term = factor `chainl1` op
    where op = (Mult <$ reservedOp "*")

-- factors are chained like: a * b / c
factor :: Parser Expr
factor = negation
    <|> fixedValue
    <|> parens expr     -- e.g. (2 + 3) * 4
    <|> try funCall     -- e.g. print("foo")
    <|> try arrAccess   -- e.g. arr[2]
    <|> Var <$> name

-- parses a negation
negation :: Parser Expr
negation = Neg 
    <$ reservedOp "-" <*> operation

-- parses a fixed value
fixedValue :: Parser Expr
fixedValue = Parser.Fixed 
    <$> value 

-- parses a function call
funCall :: Parser Expr
funCall = FunCall 
    <$> name <*> parens args
    
-- parses an access to an array element
arrAccess :: Parser Expr
arrAccess = ArrAccess 
    <$> name <*> brackets integer

-----------------------------------------------------------------------------
-- data type parsers
-----------------------------------------------------------------------------

type ArrSize = Integer

data DataType
    = CharType
    | BoolType
    | IntType
    | ArrType DataType ArrSize
    deriving (Show, Eq)

-- parses either a basic data type or an array
dataType :: Parser DataType
dataType = foldl ArrType <$> baseType <*> arrDecl
    where arrDecl = many $ brackets $ integer 

-- parses a basic data type
baseType :: Parser DataType
baseType = 
        CharType   <$ reserved "Char"
    <|> BoolType   <$ reserved "Bool"
    <|> IntType    <$ reserved "Int"

-----------------------------------------------------------------------------
-- value parsers
-----------------------------------------------------------------------------

data Value
    = Bool   Bool 
    | Char   Char
    | Int    Integer
    | Arr    [Value]
    deriving (Eq)

instance Show Value where
    show (Bool False)   = "false"
    show (Bool True)    = "true"
    show (Char val)     = show val
    show (Int val)      = show val
    show (Arr vals)     = arrToStr vals

-- returns a string representation of an array
arrToStr :: [Value] -> String
arrToStr vals = if isText vals 
    then arrToText vals 
    else "[" ++ (intercalate ", " $ show <$> vals) ++ "]"

-- converts an array of values to a text
arrToText :: [Value] -> String
arrToText [] = []
arrToText ((Char c):xs) = c : arrToText xs

-- returns true if the given values are all chars
isText :: [Value] -> Bool
isText [] = True
isText ((Char c):xs) = isText xs
isText _ = False

-- parses a value
value :: Parser Value
value = Int   <$> integer
    <|> Char  <$> character
    <|> Arr   <$> text
    <|> Arr   <$> array
    <|> Bool  <$> boolean

-- parses a string
text :: Parser [Value]
text = between (char '"') (char '"') (many (Char <$> textChar))
    where 
        textChar = escapeChar <|> noneOf "\"\\"
        escapeChar = char '\\' *> oneOf "\"\\"

-- parses a character
character :: Parser Char
character = between (char '\'') (char '\'') anyChar

-- parses an array
array :: Parser [Value]
array = brackets $ value `sepBy` comma

-- parses a boolean value
boolean :: Parser Bool
boolean = False <$ reserved "false"
      <|> True  <$ reserved "true"

-----------------------------------------------------------------------------
-- other parsers
-----------------------------------------------------------------------------

type VarDef = (VarName, DataType)

type ArgsDef = [VarDef]

-- parses a variable definition
varDef :: Parser VarDef
varDef = (,) <$> name <*> typeDecl
    where typeDecl = colon *> dataType

-- parses arguments' definition
argsDef :: Parser ArgsDef
argsDef = varDef `sepBy` comma

-- parses arguments as expressions
args :: Parser [Expr]
args = expr `sepBy` comma

-- maybe parses (on failure returns Nothing)
nullable :: Parser a -> Parser (Maybe a)
nullable p = Just <$> p <|> pure Nothing

-----------------------------------------------------------------------------
-- script generator
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
            Action      <$> QC.arbitrary 
        ]

instance QC.Arbitrary Expr where
    arbitrary = QC.sized expr
        where
            expr 0 = Parser.Fixed <$> QC.arbitrary
            expr n = let nextExpr = expr (n `div` 2) in QC.oneof [
                    FunCall <$> QC.arbitrary <*> QC.resize (n `div` 2) QC.arbitrary,
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
                    Parser.Fixed <$> QC.arbitrary 
                ]

instance QC.Arbitrary DataType where
    arbitrary = QC.oneof [
            pure BoolType,
            pure IntType,
            ArrType <$> QC.arbitrary <*> QC.arbitrary 
        ]

instance QC.Arbitrary Value where
    arbitrary = QC.oneof [
            Char <$> QC.arbitrary,
            Bool <$> QC.arbitrary,
            Int  <$> QC.arbitrary,
            Arr  <$> QC.arbitrary
        ]

instance QC.Arbitrary LoopIter where
    arbitrary = QC.oneof [ IterRange <$> QC.arbitrary <*> QC.arbitrary ]

autoScript :: IO Script
autoScript = QC.generate (QC.resize 3 QC.arbitrary)