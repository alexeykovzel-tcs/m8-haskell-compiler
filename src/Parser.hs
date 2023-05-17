module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Test.Evaluator
import Lexer


-------- STATEMENTS --------

type Script = [Statement]

data Statement
    = VarInit       Variable Expr
    | VarDecl       Variable
    | VarAssign     String Expr
    | ArrUpdate     String Integer Expr
    | FunDef        String ArgsDef DataType Script
    | ForLoop       Variable LoopRange Script
    | WhileLoop     Expr Script
    | CondIf        Expr Script
    | CondIfElse    Expr Script Script
    | StructDef     String ArgsDef
    | Return        Expr
    | Exec          Expr
    deriving Show

data LoopRange
    = FromTo        Integer Integer
    | ExprRange     Expr
    deriving Show

script :: Parser Script
script = many statement <?> "statement"

statement :: Parser Statement
statement 
    =   (varInit         <?> "variable initialization")
    <|> (varDecl         <?> "variable declaration")
    <|> (varAssign       <?> "variable assignment")
    <|> (arrUpdate       <?> "array update")
    <|> (funDef          <?> "function definition")
    <|> (forLoop         <?> "for loop")
    <|> (whileLoop       <?> "while loop")
    <|> (condIfElse      <?> "if else condition")
    <|> (condIf          <?> "if condition")
    <|> (structDef       <?> "struct definition")
    <|> (exec            <?> "execution")

-- e.g. let x: Int = 2;
varInit :: Parser Statement
varInit = VarInit 
    <$  reserved "let" <*> variable
    <*  symbol "=" <*> expr 
    <*  symbol ";"

-- e.g. let x: Int;
varDecl :: Parser Statement
varDecl = VarDecl
    <$  reserved "let" <*> variable
    <*  symbol ";"

-- e.g. x = 3 + y;
varAssign :: Parser Statement
varAssign = VarAssign 
    <$> name
    <*  symbol "=" <*> expr 
    <*  symbol ";"

-- e.g. arr[2] = x + 3;
arrUpdate :: Parser Statement
arrUpdate = ArrUpdate 
    <$> name
    <*> brackets int 
    <*  symbol "=" <*> expr 
    <*  symbol ";"

-- e.g. fun increment(x: Int) { return x + 1; }
funDef :: Parser Statement
funDef = FunDef 
    <$  reserved "fun" <*> name 
    <*> parens argsDef <* symbol "->" <*> dataType 
    <*> braces script

-- e.g. for x: Int in 2..10 { print(x); }
forLoop :: Parser Statement
forLoop = ForLoop 
    <$  reserved "for" <*> variable 
    <*  symbol "in" <*> loopRange 
    <*> braces script

loopRange :: Parser LoopRange
loopRange 
    =   FromTo      <$> int <* symbol ".." <*> int
    <|> ExprRange   <$> expr

-- e.g. while x < 3 { print(x); }
whileLoop :: Parser Statement
whileLoop = WhileLoop 
    <$  reserved "while" <*> expr 
    <*> braces script

-- e.g. if x < y { print(x); } else { print(y); }
condIfElse :: Parser Statement
condIfElse = CondIfElse 
    <$ reserved "if" <*> expr 
    <*> braces script 
    <*  reserved "else" 
    <*> braces script

-- e.g. if x < y { print(x); }
condIf :: Parser Statement
condIf = CondIf
    <$  reserved "if" <*> expr 
    <*> braces script

-- e.g. struct Person { first_name: String }
structDef :: Parser Statement
structDef = StructDef 
    <$  reserved "struct" <*> name 
    <*> braces argsDef
    <*  symbol ";"

-- e.g. return x;
return :: Parser Statement
return = Return
    <$ reserved "return" <*> expr
    <*  symbol ";"

-- e.g. print(x);
exec :: Parser Statement
exec = Exec 
    <$> expr 
    <* symbol ";"


-------- EXPRESSIONS --------

data Expr
    = Const         Value
    | Var           String
    | Calc          Expr Op Expr
    | FunCall       String [Expr]
    | StructInit    String [Expr]
    | Ternary       Expr Expr Expr
    | Lambda        ArgsDef Script
    | None
    deriving Show

expr :: Parser Expr
expr 
    =   (parens expr        <?> "parentheses")
    <|> (none               <?> "none")
    <|> (structInit         <?> "struct initialization")
    <|> (lambda             <?> "lambda expression")
    <|> (ternary            <?> "ternary operator")
    <|> (operation          <?> "operation")
    <|> (funCall            <?> "function call")
    <|> (Const <$> value    <?> "value")
    <|> (Var <$> name       <?> "variable")

-- a.k.a. null
none :: Parser Expr
none = None <$ symbol "None"

-- e.g. Person { "John", "Smith" }
structInit :: Parser Expr
structInit = StructInit 
    <$> name 
    <*> braces args

-- e.g. (x: Int) -> { x = x + 1; }
lambda :: Parser Expr
lambda = Lambda 
    <$> parens argsDef 
    <*  symbol "->" 
    <*> braces script

-- e.g. (x % 2 == 0) ? true : false
ternary :: Parser Expr
ternary = Ternary 
    <$> expr 
    <*  symbol "?" <*> expr 
    <*  symbol ":" <*> expr

-- e.g. x + y
operation :: Parser Expr
operation = Calc 
    <$> expr <*> op <*> expr

-- e.g. print("Hello")
funCall :: Parser Expr
funCall = FunCall 
    <$> name <*> parens args


-------- OPERATORS --------

data Op
    = And | Or
    | Eq  | EqOrMore | EqOrLess | More | Less
    | Add | Sub | Mult | Div | Mod
    deriving Show

op :: Parser Op
op 
    =   And         <$ reservedOp "&&"
    <|> Or          <$ reservedOp "||"
    <|> Eq          <$ reservedOp "=="
    <|> EqOrMore    <$ reservedOp ">="
    <|> EqOrLess    <$ reservedOp "<="
    <|> More        <$ reservedOp ">"
    <|> Less        <$ reservedOp "<"
    <|> Add         <$ reservedOp "+"
    <|> Sub         <$ reservedOp "-"
    <|> Mult        <$ reservedOp "*"
    <|> Div         <$ reservedOp "/"
    <|> Mod         <$ reservedOp "%"


-------- ARGUMENTS --------

type ArgsDef = [Variable]

argsDef :: Parser ArgsDef
argsDef = variable `sepBy` (symbol ",")

args :: Parser [Expr]
args = expr `sepBy` (symbol ",")


-------- VALUES --------

data DataType
    = StrType
    | BoolType
    | IntType
    | ArrType
    | StructType
    deriving Show

data Variable
    = StaticVar     String DataType
    | DynamicVar    String
    deriving Show

data Value
    = Str           String 
    | Bool          Bool 
    | Int           Integer
    | Arr           [Value]
    deriving Show

dataType :: Parser DataType
dataType 
    =   StrType     <$ reserved "String"
    <|> BoolType    <$ reserved "Bool"
    <|> IntType     <$ reserved "Int"
    <|> StructType  <$ name

variable :: Parser Variable
variable 
    =   StaticVar   <$> name <* symbol ":" <*> dataType
    <|> DynamicVar  <$> name
    
value :: Parser Value
value 
    =   Str         <$> str
    <|> Bool        <$> bool
    <|> Int         <$> int
    <|> Arr         <$> arr

str :: Parser String
str = between (char '"') (char '"') 
        (many $ noneOf "\"")

bool :: Parser Bool
bool =  False       <$ reserved "false"
    <|> True        <$ reserved "true"

arr :: Parser [Value]
arr = brackets $ value `sepBy` (symbol ",")


-------- HELPER FUNCTIONS --------

braces :: Parser a -> Parser a 
braces p = between (symbol "{") (symbol "}") p

brackets :: Parser a -> Parser a
brackets p = between (symbol "[") (symbol "]") p