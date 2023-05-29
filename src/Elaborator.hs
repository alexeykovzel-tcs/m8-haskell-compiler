module Elaborator where

import Parser (DataType)

-- data DataType
--     = StrType
--     | BoolType
--     | IntType
--     | ArrType
--     | StructType
--     deriving Show

data Attr
    = Type DataType
    deriving Show

data Tree 
    = Leaf Attr
    | Node Attr Tree Tree
    deriving Show

traverse :: Script -> Tree
traverse script = Leaf