module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Maybe
import Lexer


{-
    TODO: Write documentation...
-}


{------------------------}
{-        @SCRIPT       -}
{------------------------}


type Script = [Statement]

data Statement
    = DeclareVar    VarDef (Maybe Expr)
    | AssignVar     String Expr
    | InsertArray   String Integer Expr
    | DefFunction   String ArgsDef DataType Script
    | DefStruct     String ArgsDef
    | LoopFor       VarDef LoopIter Script
    | LoopWhile     Expr Script
    | Condition     Expr Script (Maybe Script)
    | ReturnValue   Expr
    | Action        Expr
    deriving Show

data LoopIter
    = IterRange     Integer Integer
    | IterArray     [Value]
    | IterVar       String
    deriving Show


script :: Parser Script
script = whiteSpace *> many statement


statement :: Parser Statement
statement 
    =   declareVar
    <|> try assignVar
    <|> try insertArray
    <|> defFunction
    <|> defStruct
    <|> loopFor
    <|> loopWhile
    <|> condition
    <|> returnValue
    <|> action


-- e.g. let x: Int;
declareVar :: Parser Statement
declareVar = DeclareVar
    <$  reserved "let" <*> varDef
    <*> (Just 
        <$> (symbol "=" *> expr ) 
        <|> pure Nothing)
    <*  semi


-- e.g. x = 3 + y;
assignVar :: Parser Statement
assignVar = AssignVar 
    <$> name
    <*  symbol "=" <*> expr 
    <*  semi


-- e.g. arr[2] = x + 3;
insertArray :: Parser Statement
insertArray = InsertArray 
    <$> name
    <*> brackets intValue 
    <*  symbol "=" <*> expr 
    <*  semi


-- e.g. fun increment(x: Int) { return x + 1; }
defFunction :: Parser Statement
defFunction = DefFunction
    <$  reserved "fun" <*> name 
    <*> parens argsDef <* symbol "->" <*> dataType 
    <*> braces script


-- e.g. for x: Int in 2..10 { print(x); }
loopFor :: Parser Statement
loopFor = LoopFor 
    <$  reserved "for" <*> varDef
    <*  symbol "in" <*> loopIter
    <*> braces script


loopIter :: Parser LoopIter
loopIter
    =   IterRange   <$> intValue <* symbol ".." <*> intValue
    <|> IterArray   <$> array
    <|> IterVar     <$> name


-- e.g. while x < 3 { print(x); }
loopWhile :: Parser Statement
loopWhile = LoopWhile 
    <$  reserved "while" <*> expr 
    <*> braces script


-- e.g. if x < y { print(x); } else { print(y); }
condition :: Parser Statement
condition = Condition 
    <$  reserved "if" <*> expr 
    <*> braces script
    <*> (Just 
        <$> (reserved "else" *> braces script)
        <|> pure Nothing)


-- e.g. struct Person { first_name: String }
defStruct :: Parser Statement
defStruct = DefStruct 
    <$  reserved "struct" <*> name 
    <*> braces argsDef


-- e.g. return x;
returnValue :: Parser Statement
returnValue = ReturnValue
    <$  reserved "return" <*> expr
    <*  semi


-- e.g. print(x);
action :: Parser Statement
action = Action 
    <$> expr 
    <*  semi


{------------------------}
{-     @EXPRESSIONS     -}
{------------------------}


data Expr
    = CallFunction      String [Expr]
    | DeclareStruct     String [Expr]
    | TernaryOp         Expr Expr Expr
    | Lambda            ArgsDef Script
    | Both              Expr Expr
    | OneOf             Expr Expr
    | IfEq              Expr Expr
    | IfMoreOrEq        Expr Expr
    | IfLessOrEq        Expr Expr
    | IfMore            Expr Expr
    | IfLess            Expr Expr
    | Add               Expr Expr
    | Substract         Expr Expr
    | Multiply          Expr Expr
    | Divide            Expr Expr
    | Modulo            Expr Expr
    | Variable          String
    | Fixed             Value
    deriving Show


expr :: Parser Expr
expr = try ternaryOp <|> operation


-- e.g. (x % 2 == 0) ? true : false
ternaryOp :: Parser Expr
ternaryOp = TernaryOp 
    <$> operation 
    <*  symbol "?" <*> operation 
    <*  symbol ":" <*> operation


operation :: Parser Expr
operation = logicand `chainl1` op
    where op 
            =   (Both           <$ reservedOp "&&") 
            <|> (OneOf          <$ reservedOp "||")


-- logicand && logicand || logicand 
logicand :: Parser Expr
logicand = comparand `chainl1` op
    where op 
            =   (IfEq           <$ reservedOp "==")
            <|> (IfMoreOrEq     <$ reservedOp ">=")
            <|> (IfLessOrEq     <$ reservedOp "<=")
            <|> (IfMore         <$ reservedOp ">")
            <|> (IfLess         <$ reservedOp "<")


-- comparand == comparand 
comparand :: Parser Expr
comparand = term `chainl1` op
    where op 
            =   (Add            <$ reservedOp "+")
            <|> (Substract      <$ reservedOp "-")


-- term + term - term
term :: Parser Expr
term = factor `chainl1` op
    where op 
            =   (Multiply       <$ reservedOp "*")
            <|> (Divide         <$ reservedOp "/")
            <|> (Modulo         <$ reservedOp "%")


-- factor * factor / factor
factor :: Parser Expr
factor 
    =   Fixed <$> value
    <|> try lambda
    <|> parens expr
    <|> try callFunction
    <|> try declareStruct
    <|> Variable <$> name


-- e.g. (x: Int) -> { x = x + 1; }
lambda :: Parser Expr
lambda = Lambda 
    <$> parens argsDef 
    <*  symbol "->" 
    <*> braces script


-- e.g. print("Hello")
callFunction :: Parser Expr
callFunction = CallFunction 
    <$> name <*> parens args


-- e.g. Person { "John", "Smith" }
declareStruct :: Parser Expr
declareStruct = DeclareStruct 
    <$> name 
    <*> braces args


{------------------------}
{-         @ARGS        -}
{------------------------}


type ArgsDef = [VarDef]

-- x: Int, y: Int
argsDef :: Parser ArgsDef
argsDef = varDef `sepBy` comma


-- 2 + 3, 5, "Hello" 
args :: Parser [Expr]
args = expr `sepBy` comma


{------------------------}
{-        @VALUES       -}
{------------------------}


data VarDef 
    = VarDef String DataType
    deriving Show

data Value
    = Str           String 
    | Bool          Bool 
    | Int           Integer
    | Array         [Value]
    | None
    deriving Show

data DataType
    = StrType
    | BoolType
    | IntType
    | ArrayType
    | StructType
    deriving Show


dataType :: Parser DataType
dataType 
    =   StrType     <$ reserved "String"
    <|> BoolType    <$ reserved "Bool"
    <|> IntType     <$ reserved "Int"
    <|> StructType  <$ name


varDef :: Parser VarDef
varDef = VarDef 
    <$> name <* colon
    <*> dataType


value :: Parser Value
value 
    =   Str         <$> strValue
    <|> Int         <$> intValue
    <|> Array       <$> array
    <|> Bool        <$> boolValue
    <|> None        <$  symbol "None"


-- e.g. "Hello World"
strValue :: Parser String
strValue = between (char '"') (char '"') (many strChar)
    where 
        strChar = escapeChar <|> noneOf "\"\\"
        escapeChar = char '\\' *> oneOf "\"\\"


-- true or false
boolValue :: Parser Bool
boolValue
    =   False       <$ reserved "false"
    <|> True        <$ reserved "true"


-- e.g. [1, 2, 3]
array :: Parser [Value]
array = brackets $ value `sepBy` comma