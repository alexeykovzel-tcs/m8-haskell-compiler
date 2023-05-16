module Parser where

import Text.ParserCombinators.Parsec
import Lexer

type Script = [Statement]

type ArgsDef = [(String, DataType)]

data DataType 
    = String 
    | Bool 
    | Int 
    | Array 
    | Struct
    deriving Show

data Value = 
    StrVal String 
    | BoolVal Bool 
    | IntVal Integer
    deriving Show

data Operator
    = And | Or
    | Eq | EqOrMore | EqOrLess | More | Less
    | Add | Sub | Mult | Div | Mod
    deriving Show

data Range
    = FromTo        Integer Integer
    | OfArray       [Value]
    deriving Show

data Statement
    = VarInit       String DataType Expr
    | VarDecl       String DataType
    | VarAssign     String Expr
    | ArrUpdate     String Integer Expr
    | FunDef        String ArgsDef DataType Script Expr
    | LoopFor       Range Script
    | LoopWhile     Expr Script
    | CondIf        Expr Script
    | CondIfElse    Expr Script Script
    | StructDef     String ArgsDef
    | Exec          Expr
    deriving Show

data Expr
    = Const         Value
    | Var           Value
    | Calc          Operator Expr Expr
    | FunCall       String Expr
    | StructInit    String [Value]
    | Ternary       Expr Expr Expr
    | Lambda        ArgsDef Script
    deriving Show


-- parseProgram :: Parser Script
-- parseProgram = ...

-- parseScript :: Parser Script
-- parseScript = ...

-- parseFun :: Parser Expr
-- parseFun = ...

-- parseArgsDef :: Parser ArgsDef
-- parseArgsDef = ...

-- parseArgs :: Parser [Expr]
-- parseArgs = ...

-- parseExpr :: Parser Expr
-- parseExpr = ...

-- parseOp :: Parser Op
-- parseOp = ...

-- parseBool :: Parser Expr
-- parseBool = ...

-- parseString :: Parser Expr
-- parseString = ...

-- parseArray :: Parser [Expr]
-- parseArray = ...

-- parseElse :: Parser ...
-- parseElse = ...

-- parseRange :: Parser Range
-- parseRange = ...