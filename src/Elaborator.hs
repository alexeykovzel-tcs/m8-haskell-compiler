module Elaborator (
    Depth, Offset,
    ScopeID, Scope,
    Context,
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

type ElabResult = Either Error Context

data Context = Context Scope VarMap deriving Show

-- used to determine a variable position in memory
type VarMap = Map.Map Scope (Map.Map VarName VarCoord)
type VarCoord = (Depth, Offset)

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | EmptyDecl     SourcePos String    -- variable decl. with no type and value
    | NoReturn      SourcePos           -- function decl. without return
    deriving Show

initScope :: Scope
initScope = (0, 0)

initContext :: VarMap
initContext = Map.insert initScope Map.empty (Map.empty)

incScope :: Scope -> Scope
incScope (a, b) = (a+1, b)

decScope :: Scope -> Scope
decScope (a, b) = (a-1, b)

incDepth :: Scope -> Scope
incDepth (a, b) = (a, b+1)

insertVar :: Scope -> VarName -> VarCoord -> VarMap -> VarMap
insertVar scope varName varCoord varMap
            = Map.insert scope (Map.insert varName varCoord (Map.findWithDefault Map.empty scope varMap)) varMap

findVar :: Scope -> String -> VarMap -> Bool
findVar scope varName varMap
        | isJust query = True
        | otherwise = False
        where query = Map.lookup varName (fromJust (Map.lookup scope varMap))

traverseContext :: Scope -> VarName -> VarMap -> Bool
traverseContext (-1,0) _ _ = False
traverseContext scope varName varMap
                      | findVar scope varName varMap = True
                      | otherwise = traverseContext (decScope scope) varName varMap

-- TODO: get right position
initElaborate :: Script -> ElabResult -> ElabResult
initElaborate [] context = error $ show context
initElaborate ((VarDecl def maybeExpr):xs) (Right (Context scope varMap)) = initElaborate xs $ Right $ Context scope (insertVar scope (fst def) (1,1) varMap)
initElaborate ((VarAssign name expr):xs) context@(Right (Context scope varMap))
                                        | traverseContext scope name varMap = initElaborate xs context
                                        | otherwise = Left $ MissingDecl (newPos "" 1 1) "Calling non-existent entity!"

elaborate :: Script -> ElabResult
elaborate script = initElaborate script (Right $ Context initScope initContext)

-- Should give an error because y wasn't declared
steasy :: Script
steasy = tryParse script "let x = 2; x = 4; y = 5;"

-- Should give an error because y in different scope + should change x = 1 from scope (0,0)
sthard :: Script
sthard = tryParse script "let x = 2; if true { let x = 5; let y = 2; } else { y = 4; for i in 1..3 { x = 1; } }"
