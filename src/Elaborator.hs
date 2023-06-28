module Elaborator (
    Depth, Offset,
    ScopeID, Scope,
    Overview,
    FunMap,
    VarMap,
    VarCoord,
    elaborate
) where

import Parser
import Data.Maybe
import Data.Either
import Text.Parsec.Pos
import qualified Data.Map as Map

type Offset = Int
type Depth = Int

type ScopeID = Integer
type Scope = (ScopeID, Depth)

data Overview = Overview FunMap VarMap

-- used to generate code from a function call 
type FunMap = Map.Map FunName Statement

-- used to determine a variable position in memory
type VarMap = Map.Map ScopeID (Map.Map VarName VarCoord)
type VarCoord = (Depth, Offset)

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | EmptyDecl     SourcePos String    -- variable decl. with no type and value
    | NoReturn      SourcePos           -- function decl. without return



elaborate :: Script -> Either Error Overview
elaborate script = error "not defined"