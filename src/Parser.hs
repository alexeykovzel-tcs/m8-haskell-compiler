{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE FlexibleInstances #-}

module Parser where

{- author: Aliaksei Kouzel - s2648563 -}

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Lexer
import Test.QuickCheck
import Control.Exception

-----------------------------------------------------------------------------
-- parser usage
-----------------------------------------------------------------------------

-- parseWith script    "let x: Int = 2 + 1";
-- parseWith script    "for i: Int in [1, 2, 3] { print(i); }"
-- parseWith expr      "2 * (3 + 4) / 6"

parseScript :: String -> Script
parseScript code = parseWith script code

-- parsers a script in a file
parseFile :: FilePath -> IO Script
parseFile file = parseWith script <$> readFile file

-- parses a string with the given parser
parseWith :: Parser a -> String -> a
parseWith p code = either (error . show) id $ parse p "" code

-----------------------------------------------------------------------------
-- statement parsing
-----------------------------------------------------------------------------

type FunName = String
type VarName = String

type Script = [Statement]

data Statement
    = VarDecl       VarDef (Maybe Expr)
    | GlVarDecl     VarDef (Maybe Expr)
    | VarAssign     VarName Expr
    | ArrInsert     VarName Integer Expr
    | FunDef        FunName ArgsDef (Maybe DataType) Script
    | ForLoop       VarDef LoopIter Script
    | WhileLoop     Expr Script
    | Condition     Expr Script (Maybe Script)
    | Parallel      Integer Script
    | InScope       Script
    | ReturnVal     Expr
    | Action        Expr
    deriving (Show, Eq)

data LoopIter 
    = IterRange Expr Expr
    deriving (Show, Eq)

script :: Parser Script
script = whiteSpace *> many statement

statement :: Parser Statement
statement =   
        varDecl         -- e.g. let x: Int;
    <|> glVarDecl       -- e.g. global x: Int;
    <|> Text.ParserCombinators.Parsec.try varAssign   -- e.g. x = 3 + y;
    <|> Text.ParserCombinators.Parsec.try arrInsert   -- e.g. arr[2] = x + 3;
    <|> inScope         -- e.g. { ... }
    <|> funDef          -- e.g. fun incr(x: Int) -> Int { ... }
    <|> forLoop         -- e.g. for x: Int in 2..10 { ... }
    <|> whileLoop       -- e.g. while x < 3 { ... }
    <|> condition       -- e.g. if x < y { ... } else { ... }
    <|> parallel        -- e.g. parallel 4 { ... }
    <|> returnVal       -- e.g. return x;
    <|> action          -- e.g. print(x);

-- parses a local variable declaration
varDecl :: Parser Statement
varDecl = VarDecl
    <$  reserved "let" <*> varDef
    <*> nullable (symbol "=" *> expr)
    <*  semi

-- parses a global variable declaration
glVarDecl :: Parser Statement
glVarDecl = GlVarDecl
    <$  reserved "global" <*> varDef
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

-- parses an inner scope
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

-- parses a for loop
forLoop :: Parser Statement
forLoop = ForLoop 
    <$  reserved "for" <*> varDef
    <*  symbol "in" <*> loopIter
    <*> braces script

-- parses a loop iterator
loopIter :: Parser LoopIter
loopIter = IterRange 
    <$> expr <* symbol ".." <*> expr

-- parses a while loop
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

-- parses a parallel execution
parallel :: Parser Statement
parallel = Parallel
    <$  reserved "parallel" 
    <*> integer
    <*> braces script

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
    deriving (Show, Eq)

-- parses an expression
expr :: Parser Expr
expr =  Text.ParserCombinators.Parsec.try ternary 
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

-- parsers logicands chained like: a && b || c 
logicand :: Parser Expr
logicand = comparand `chainl1` op
    where op = (Eq      <$ reservedOp "==")
           <|> (MoreEq  <$ reservedOp ">=")
           <|> (LessEq  <$ reservedOp "<=")
           <|> (More    <$ reservedOp ">")
           <|> (Less    <$ reservedOp "<")

-- parsers comparands chained like: a == b 
comparand :: Parser Expr
comparand = term `chainl1` op
    where op = (Add <$ reservedOp "+")
           <|> (Sub <$ reservedOp "-")

-- parsers terms chained like: a + b - c
term :: Parser Expr
term = factor `chainl1` op
    where op = (Mult <$ reservedOp "*")

-- parsers factors chained like: a * b / c
factor :: Parser Expr
factor = negation
    <|> fixedValue
    <|> parens expr     -- e.g. (2 + 3) * 4
    <|> Text.ParserCombinators.Parsec.try funCall     -- e.g. print("foo")
    <|> Text.ParserCombinators.Parsec.try arrAccess   -- e.g. arr[2]
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
    deriving (Eq)

instance Show DataType where
    show CharType = "Char"
    show BoolType = "Bool"
    show IntType = "Int"
    show (ArrType dataType _) = "[" ++ show dataType ++ "]"

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

-- converts an array of values to text
arrToText :: [Value] -> String
arrToText [] = []
arrToText ((Char c):xs) = c : arrToText xs

-- returns true if the given values are chars
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

-- parses a boolean
boolean :: Parser Bool
boolean = False <$ reserved "false"
      <|> True  <$ reserved "true"

-----------------------------------------------------------------------------
-- variable/argument parsers
-----------------------------------------------------------------------------

type VarDef = (VarName, DataType)

type ArgsDef = [VarDef]

-- parses a variable definition
varDef :: Parser VarDef
varDef = (,) <$> name <*> typeDecl
    where typeDecl = colon *> dataType

-- parses the definition of arguments
argsDef :: Parser ArgsDef
argsDef = varDef `sepBy` comma

-- parses arguments as expressions
args :: Parser [Expr]
args = expr `sepBy` comma

-----------------------------------------------------------------------------
-- pretty printer
-----------------------------------------------------------------------------

class PrettyPrint a where
    pretty :: a -> String

instance PrettyPrint Script where
    pretty = unwords . map pretty

instance PrettyPrint Statement where
    pretty (Action expr)     = unwords [pretty expr, ";"]
    pretty (ReturnVal expr)  = unwords ["return", pretty expr, ";"]    
    
    pretty (VarDecl (name, dataType) expr) =
        unwords ["let", name, ":", pretty dataType, "=", pretty expr, ";"]
    
    pretty (GlVarDecl (name, dataType) expr) =
        unwords ["global", name, ":", pretty dataType, "=", pretty expr, ";"]
    
    pretty (VarAssign name expr) =
        unwords [name, "=", pretty expr, ";"]
    
    pretty (ArrInsert name index expr) =
        unwords [name, "[", show index, "] =", pretty expr, ";"]
    
    pretty (FunDef name args retType body) =
        unwords ["fun", name, "(", pretty args, ")", 
                pretty retType, "{", pretty body, "}"]
    
    pretty (ForLoop iterName iter body) =
        unwords ["for", pretty iterName, ": Int in", 
                pretty iter, "{", pretty body, "}"]
    
    pretty (WhileLoop cond body) =
        unwords ["while", pretty cond, "{", pretty body, "}"]

    pretty (Condition cond ifBody Nothing) = 
        unwords ["if", pretty cond, "{", pretty ifBody, "}"]

    pretty (Condition cond ifBody (Just elseBody)) =
        unwords ["if", pretty cond, "{", pretty ifBody, 
                "} else {", pretty elseBody, "}"]
    
    pretty (Parallel num body) =
        unwords ["parallel", show num, "{", pretty body, "}"]
    
instance PrettyPrint (Maybe Script) where
    pretty (Just script) = pretty script
    pretty Nothing = ""

instance PrettyPrint LoopIter where
    pretty (IterRange from to) = 
        pretty from ++ ".." ++ pretty to

instance PrettyPrint Expr where
    pretty (Both    e1 e2) = prettyJoin e1 " && " e2
    pretty (OneOf   e1 e2) = prettyJoin e1 " || " e2
    pretty (Eq      e1 e2) = prettyJoin e1 " == " e2
    pretty (MoreEq  e1 e2) = prettyJoin e1 " >= " e2
    pretty (LessEq  e1 e2) = prettyJoin e1 " <= " e2
    pretty (More    e1 e2) = prettyJoin e1 " > "  e2
    pretty (Less    e1 e2) = prettyJoin e1 " < "  e2
    pretty (Add     e1 e2) = prettyJoin e1 " + "  e2
    pretty (Sub     e1 e2) = prettyJoin e1 " - "  e2
    pretty (Mult    e1 e2) = prettyJoin e1 " * "  e2    

    pretty (Var name)            = name
    pretty (Parser.Fixed value)         = show value
    pretty (FunCall name args)   = show name ++ "(" ++ pretty args ++ ")"
    pretty (ArrAccess name idx)  = show name ++ "[" ++ show idx ++ "]"
    pretty (Neg expr)            = "-" ++ pretty expr

    pretty (Ternary cond e1 e2) = 
        unwords [pretty cond, "?", pretty e1, ":", pretty e2]

instance PrettyPrint Value where
    pretty (Bool value) = show value
    pretty (Char value) = show value
    pretty (Int value)  = show value
    pretty (Arr value)  = show value

instance PrettyPrint Integer where
    pretty = show

instance PrettyPrint [Expr] where
    pretty = unwords . map pretty

instance PrettyPrint FunName where
    pretty x = x

instance PrettyPrint (Maybe DataType) where
    pretty (Just dataType) = pretty dataType
    pretty Nothing = ""

instance PrettyPrint DataType where
    pretty CharType = "Char"
    pretty BoolType = "Bool"
    pretty IntType  = "Int"
    pretty (ArrType size dataType) = 
        "[" ++ pretty size ++ "]: " ++ pretty dataType

instance PrettyPrint ArgsDef where
    pretty = unwords . map pretty

instance PrettyPrint VarDef where
    pretty (name, dataType) = 
        pretty name ++ ": " ++ pretty dataType

instance PrettyPrint (Maybe Expr) where
    pretty (Just expr) = pretty expr
    pretty Nothing = ""

prettyJoin :: PrettyPrint a => a -> String -> a -> String
prettyJoin s1 sep s2 = pretty s1 ++ sep ++ pretty s2



-----------------------------------------------------------------------------
-- generator for the correct script
-----------------------------------------------------------------------------

data NonEmptyList a = NonEmptyList [a] deriving (Show)

-- Generates non-empty scripts
instance Arbitrary a => Arbitrary (Parser.NonEmptyList a) where
    arbitrary = Parser.NonEmptyList <$> (listOf1 arbitrary)

instance Arbitrary Statement where
    arbitrary = oneof [
        VarDecl    <$> arbitrary <*> arbitrary,
        GlVarDecl  <$> arbitrary <*> arbitrary,
        VarAssign  <$> arbitrary <*> arbitrary,
        ArrInsert  <$> arbitrary <*> arbitrary <*> arbitrary,
        FunDef     <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        ForLoop    <$> arbitrary <*> arbitrary <*> arbitrary,
        WhileLoop  <$> arbitrary <*> arbitrary,
        Condition  <$> arbitrary <*> arbitrary <*> arbitrary,
        Parallel   <$> arbitrary <*> arbitrary,
        InScope    <$> arbitrary,
        ReturnVal  <$> arbitrary,
        Action     <$> arbitrary ]

instance Arbitrary Expr where
    arbitrary = sized expr
        where
            expr 0 = Parser.Fixed <$> arbitrary
            expr n = let nextExpr = expr (n `div` 2) in oneof [
                FunCall    <$> arbitrary <*> resize (n `div` 2) arbitrary,
                ArrAccess  <$> arbitrary <*> arbitrary,
                Ternary    <$> nextExpr  <*> nextExpr <*> nextExpr,
                Both       <$> nextExpr  <*> nextExpr,
                OneOf      <$> nextExpr  <*> nextExpr,
                Eq         <$> nextExpr  <*> nextExpr,
                MoreEq     <$> nextExpr  <*> nextExpr,
                LessEq     <$> nextExpr  <*> nextExpr,
                More       <$> nextExpr  <*> nextExpr,
                Less       <$> nextExpr  <*> nextExpr,
                Add        <$> nextExpr  <*> nextExpr,
                Sub        <$> nextExpr  <*> nextExpr,
                Mult       <$> nextExpr  <*> nextExpr,
                Neg        <$> nextExpr,
                Var        <$> arbitrary,
                Parser.Fixed  <$> arbitrary ]

instance Arbitrary DataType where
    arbitrary = oneof [
        pure CharType,
        pure BoolType,
        pure IntType,
        ArrType <$> arbitrary <*> arbitrary ]

instance Arbitrary Value where
    arbitrary = oneof [
        Bool <$> arbitrary,
        Char <$> arbitrary,
        Int  <$> arbitrary,
        Arr  <$> arbitrary ]

instance Arbitrary LoopIter where
    arbitrary = IterRange <$> arbitrary <*> arbitrary

prop_script :: Property
prop_script = forAll autoScript 
        $ \(NonEmptyList script) -> parseScript (pretty script) == script


autoScript :: Gen (Parser.NonEmptyList Statement)
autoScript = NonEmptyList <$> resize 3 arbitrary

-----------------------------------------------------------------------------
-- utils
-----------------------------------------------------------------------------

-- a helper function to check if an error is thrown.
throwsError :: a -> IO Bool
throwsError x = catch (evaluate x >> return False) handler

handler :: ErrorCall -> IO Bool
handler _ = return True

-- invalid test inputs
invalidInputs :: [String]
invalidInputs = 
    [
        "let x = 1;", -- VarDecl
        "let x Int = 1;", -- VarDecl
        "let x: Int = ;", -- VarDecl
        "global x = 1;", -- GlVarDecl
        "global x Int = 1;", -- GlVarDecl
        "global x: Int = ;", -- GlVarDecl
        "x  1;", -- VarAssign
        "x -> 1;", -- VarAssign
        "x = ;", -- VarAssign
        "x[1] -> 1;", -- ArrInsert
        "x[1] = ;", -- ArrInsert
        "x[1] : Int = 1;", -- ArrInsert
        "x[1] : 1;", -- ArrInsert
        "def f() -> Int { return 1; }", -- FunDef
        "fun f() -> x { return 1; }", -- FunDef
        "fun f -> Int { return 1; }", -- FunDef
        "for x in 1..10 { return 1; }", -- ForLoop
        "for x: Int in 1.. { return 1; }", -- ForLoop
        "for x in 1.. { return 1; }", -- ForLoop
        "while (x  3) { return 1; }", -- WhileLoop
        "while x: Int { return 1; }", -- WhileLoop
        "if (x  3) { return 1; }", -- Condition
        "if x: Int { return 1; }", -- Condition
        "par { return 1; }", -- Parallel
        "parallel { return 1; }", -- Parallel
        "return x: Int;", -- ReturnVal
        "print(Int);", -- Action
        "let x: Int = 1; print(x); parallel { return 1; }", -- Script
        "let x = 1, print(x);",
        "let x = 1 print(x)",
        "print(;",
        "let x = ; print(x)",
        "let x = 1; let x = 2;",
        "let x = 2; let y = x + ;",
        "let x = 3; if (x >) { print(x); }",
        "let x = 4; while (x >) { print(x); }",
        "let x = 5; for (x in..31) { print(x); }",
        "let x = 6; def f() { return x +; }",
        "let x = 7; def f(x) { return x +; }",
        "let x = 8; def f() { return ; }"
    ]

validInputs :: [String]
validInputs = 
    [
        "let x: Int = 1;", -- VarDecl
        "global x: Int = 1;", -- GlVarDecl
        "x = 1;", -- VarAssign
        "x[3] = 1;", -- ArrInsert
        "fun f() -> Int { return 1; }", -- FunDef
        "fun f() { return 1; }", -- FunDef
        "for x: Int in 1..10 { return 1; }", -- ForLoop
        "while x > 3 { return 1; }", -- WhileLoop
        "if x > 3 { return 1; }", -- Condition
        "if x > 3 { return 1; } else { return 2; }", -- Condition
        "parallel 2 { x = x + 4; }", -- Parallel
        "return 1;", -- ReturnVal
        "print(x);", -- Action
        "let x: Int = 1; print(x); parallel 2 { return 1; }", -- Script
        "let x: Int = 1; print(x);", -- Script
        "if x > 3 { return 1; } else {parallel 2 { return 2; }}", -- Script
        "let x: Int = 1; if x > 3 { return 1; } else { parallel 2 { return 2; }}", -- Script
        "for x: Int in 1..10 { return 1; } if x > 3 { return 1; } else {if x > 2 {parallel 2 { return 2; }}}" -- Script
    ]

-- test that error thrown when parsing invalid syntax with one element
prop_invalidSyntax :: Property
prop_invalidSyntax =
  forAll (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) $ \s ->
    ioProperty $ throwsError $ parseWith script s

-- test that error thrown when parsing invalid syntax of script
prop_invalidSyntaxScript :: Property
prop_invalidSyntaxScript = forAll (elements invalidInputs) $ \s ->
    ioProperty $ throwsError $ parseWith script s

-- test that no error will be thrown when given valid
prop_validSyntaxScript :: Property
prop_validSyntaxScript = forAll (elements validInputs) $ \s ->
    ioProperty $ not <$> throwsError (parseWith script s)
