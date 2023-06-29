{-# LANGUAGE FlexibleInstances #-}

module Elaborator (
    Depth, Offset,
    VarTable, VarPos,
    ScopeID,
    varTable
) where

import Parser
import Data.Maybe
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec.Pos
import Control.Monad (join)
import Table

-----------------------------------------------------------------------------
-- position variables in memory
-----------------------------------------------------------------------------

-- builds a table with memory positions for variables by scope
-- (each position contains scope depth and offset from ARP)
varTable :: Script -> VarTable
varTable script = result
    where (_, _, _, result) = allocVars (0, 0, (1, 0), []) script

testVarMap :: FilePath -> IO()
testVarMap file = join $ 
    printTable . varTable . tryParse script <$> readFile file

-----------------------------------------------------------------------------

type ScopeID  = Int
type Offset   = Int
type Depth    = Int

type VarPos   = (Depth, Offset)
type VarTable = Table ScopeID VarName VarPos
type VarCtx   = (ScopeID, ScopeID, VarPos, VarTable)

class VarAllocator a where
    allocVars :: VarCtx -> a -> VarCtx

instance VarAllocator Script where
    allocVars ctx [] = ctx
    allocVars ctx (x:xs) = allocVars (allocVars ctx x) xs

-- TODO: VarAllocator for FunDef
instance VarAllocator Statement where
    allocVars ctx (VarDecl (name, _) _)   = allocVar ctx name
    allocVars ctx (ForLoop (i, _) _ body) = allocVar (childScopes ctx [body]) i
    allocVars ctx (WhileLoop _ body)      = childScopes ctx [body]
    allocVars ctx (Condition _ a b)       = childScopes ctx $ a : maybe [] pure b
    allocVars ctx _ = ctx

-- creates new scopes at the next depth
childScopes :: VarCtx -> [Script] -> VarCtx
childScopes (scope, _, (depth, _), table) xs
    = peerScopes (scope + 1, scope, (depth + 1, 0), table) xs

-- creates new scopes at the same depth
peerScopes :: VarCtx -> [Script] -> VarCtx
peerScopes ctx [] = ctx
peerScopes ctx [script] = inheritVars ctx $ allocVars ctx script
peerScopes ctx@(scope, prevScope, pos, _) (script:scripts) 
    = peerScopes (newScope + 1, prevScope, pos, newTable) scripts
    where (newScope, _, _, newTable) = inheritVars ctx $ allocVars ctx script

-- inherits missing variable positions from the previous scope 
inheritVars :: VarCtx -> VarCtx -> VarCtx
inheritVars (scope, prevScope, _, _) (a, b, c, table) = (a, b, c, newTable)
    where 
        vars     = findRow scope table
        prevVars = findRow prevScope table
        newVars  = filterCells prevVars vars
        newTable = updateRow scope newVars table

-- adds a variable position to the current scope
allocVar :: VarCtx -> String -> VarCtx
allocVar (scope, prevScope, pos@(depth, offset), table) name = newCtx
    where 
        newTable = insertCell scope name pos table
        newCtx   = (scope, prevScope, (depth, offset + 1), newTable)

-----------------------------------------------------------------------------
-- type checking
-----------------------------------------------------------------------------

steasy :: Script
steasy = tryParse script "let x: Int = 5; x = 0; y = 5;"

type CurrScope = (Int, Int)
type PrevScope = (Int, Int)
type Scopes = (CurrScope, PrevScope)

type VarType = (VarName, Maybe DataType)
type ScopeData = Map.Map CurrScope [VarType]

type TypeChecker = Either Error VarScopes

data VarScopes = VarScopes Scopes ScopeData deriving Show

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    -- | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | EmptyDecl     SourcePos String    -- variable decl. with no type and value
    | NoReturn      SourcePos           -- function decl. without return
    deriving Show

initScope :: Scopes
initScope = ((0,0), (0,0))

incScope :: Scopes -> Scopes
incScope ((a,b), (c,d))
                | (a+1)==c = ((a+1,d+1), (a,b))
                | otherwise = ((a+1,b), (a,b))

decScope :: Scopes -> Scopes
decScope ((a,b), (c,d)) = ((a-1,b), (a,b))

initTypeChecker :: TypeChecker
initTypeChecker = Right $ VarScopes initScope Map.empty

-- Adds variable to VarScopes
addVar :: VarType -> TypeChecker -> TypeChecker
addVar varType (Right (VarScopes scopes scopeData)) 
                    = Right $ VarScopes scopes (Map.insertWith (++) (fst scopes) [varType] scopeData)

-- Traverse TypeChecker to find a variable in (wrapping) scopes
findVar :: VarName -> TypeChecker -> Maybe VarType
findVar _ (Right (VarScopes ((-1, _), _) scopeData)) = Nothing
findVar vName (Right (VarScopes scope@(currScope, _) scopeData)) 
                    | isJust varType = varType
                    | otherwise = findVar vName (Right (VarScopes (decScope scope) scopeData))
                    where types = fromJust $ getValue currScope scopeData
                          varType = find (\(x, _) -> x == vName) types

-- Checks that the VarName has proper VarType by traversing the entire map and compare types
isValidType :: VarName -> TypeChecker -> Maybe VarType
isValidType varName typeChecker = Nothing

checkTypes :: Script -> TypeChecker -> TypeChecker
checkTypes [] (Right varScopes@(VarScopes scopes scopeData)) = error $ show varScopes
checkTypes ((VarDecl varType@(vName, dataType) _):xs) typeChecker 
                          | isJust dataType = checkTypes xs (addVar varType typeChecker)
                          | otherwise = error "EmptyDecl"
checkTypes ((VarAssign vName _):xs) typeChecker 
                          | isJust (findVar vName typeChecker) = checkTypes xs typeChecker
                          | otherwise = error "MissingDecl"
checkTypes ((ArrInsert vName _ _):xs) typeChecker
                          | isJust (isValidType vName typeChecker) = checkTypes xs typeChecker
                          | otherwise = error ""

elaborate :: Script -> TypeChecker
elaborate script = checkTypes script initTypeChecker

----- UTILS -----
getValue :: CurrScope -> ScopeData -> Maybe [VarType]
getValue currScope scopeData = Map.lookup currScope scopeData
