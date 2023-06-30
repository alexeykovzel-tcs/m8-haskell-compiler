module Elaborator where

import Parser
import Text.Parsec.Pos
import Data.Maybe
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- type checking
-----------------------------------------------------------------------------

steasy :: Script
steasy = tryParse script "let x: Int = 5; x = 0; y = 5;"

type CurrScope = (Int, Int)
type PrevScope = (Int, Int)
type Scopes = (CurrScope, PrevScope)

type VarType = (VarName, Maybe DataType)
type ScopeData = Map CurrScope [VarType]

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