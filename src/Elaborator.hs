module Elaborator where

import Parser
import Text.Parsec.Pos
import Data.Maybe
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- Type Checking
-----------------------------------------------------------------------------

type CurrScope = (Int, Int)
type PrevScope = (Int, Int)
type Scopes = (CurrScope, PrevScope)

type VarType = (VarName, Maybe DataType)
type ScopeData = Map CurrScope [VarType]

type TypeChecker = Either [Error] VarScopes

data VarScopes = VarScopes Scopes ScopeData deriving Show

class Comparator a where
    comp :: Value -> a -> Bool
    assgn :: Value -> a

instance Comparator DataType where
    comp (Text _) StrType = True
    comp (Bool _) BoolType = True
    comp (Int _) IntType = True
    comp (Arr _) (ArrType _ _) = True
    comp None _ = False
    comp _ _ = False
    assgn (Text _) = StrType
    assgn (Bool _) = BoolType
    assgn (Int _) = IntType
    -- assgn (Arr _) = ArrType

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | EmptyDecl     SourcePos String    -- variable decl. with no type and value
    | NoReturn      SourcePos           -- function decl. without return
    deriving Show

addError :: Error -> TypeChecker -> TypeChecker
addError error (Left errors) = Left $ error : errors

-- Traverses TypeChecker to find a variable in (wrapping) scopes
findVar :: VarName -> TypeChecker -> Maybe VarType
findVar _ (Right (VarScopes ((-1, _), _) scopeData)) = Nothing
findVar vName (Right (VarScopes scope@(currScope, _) scopeData)) 
                    | isJust varType = varType
                    | otherwise      = findVar vName (Right (VarScopes (decScope scope) scopeData))
                    where types = fromJust $ getValue currScope scopeData
                          varType = find (\(x, _) -> x == vName) types

-- Adds variable to VarScopes
addVar :: VarType -> TypeChecker -> TypeChecker
addVar varType (Right (VarScopes scopes scopeData)) 
                    = Right $ VarScopes scopes (Map.insertWith (++) (fst scopes) [varType] scopeData)

-- Adds 
addType :: Value -> Maybe DataType
addType (Text _) = Just StrType
addType (Bool _) = Just BoolType
addType (Int _) = Just IntType
addType _ = Nothing

-- (FIX)
-- Checks that the assigned value is compatible with the type of variable
checkVarDecl :: VarType -> Maybe Expr -> TypeChecker -> TypeChecker
checkVarDecl (varName, Nothing) Nothing _ = error "EmptyDecl"
checkVarDecl (varName, dataType) Nothing typeChecker = typeChecker
checkVarDecl (varName, Nothing) (Just (Fixed (value))) typeChecker = addVar (varName, addType value) typeChecker
checkVarDecl varType@(varName, dataType) (Just (Fixed (value))) typeChecker
                | comp value (fromJust dataType) == False = error "InvalidType"
                | otherwise                               = addVar varType typeChecker 

-- checkVarAssign :: VarName -> Expr -> TypeChecker -> TypeChecker
-- checkVarAssign varName expr typeChecker
--                 | isJust (findVar varName typeChecker) == False = error "MissingDecl"
--                 | isJust dataType && comp expr dataType         = 
--                 where (_, dataType) = find varName typeChecker

-- Checks the correctness of the written program
check :: Script -> TypeChecker -> TypeChecker
check [] (Right (VarScopes scopes scopeData)) = (Right (VarScopes (decScope scopes) scopeData))
check ((VarDecl varType maybeExpr):xs) typeChecker = checkVarDecl varType maybeExpr typeChecker
check ((VarAssign vName expr):xs) typeChecker 
                          | isJust (findVar vName typeChecker) = check xs typeChecker
                          | otherwise = error "MissingDecl"
-- check ((ArrInsert vName _ expr):xs) typeChecker
--                           | isJust (isValidType vName expr typeChecker) = check xs typeChecker
--                           | otherwise = error ""
-- check ((Condition _ script maybeScript):xs) (Right (VarScopes scopes scopeData))
--                           = checkTypes xs $ check script (Right (VarScopes (incScope scopes) scopeData)) -- check maybeScript
-- checkTypes ((WhileLoop _ script):xs) (Right (VarScopes scopes scopeData))
--                           = checkTypes xs $ checkTypes script (Right (VarScopes (incScope scopes) scopeData))
-- checkTypes ((ForLoop varDef _ script):xs) (Right (VarScopes scopes scopeData))
--                           = checkTypes xs $ checkTypes script (Right (VarScopes (incScope scopes) scopeData))

-- Generates error message with multiple errors found
generateErrorMessage :: TypeChecker -> String
generateErrorMessage (Left errors) = error "Multiple Errors!"

elaborate :: Script -> TypeChecker
elaborate script = check script initTypeChecker

-----------------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------------

steasy :: Script
steasy = tryParse script "let x = 5;"

initScope :: Scopes
initScope = ((0,0), (0,0))

initTypeChecker :: TypeChecker
initTypeChecker = Right $ VarScopes initScope Map.empty

incScope :: Scopes -> Scopes
incScope ((a,b), (c,d))
                | (a+1)==c = ((a+1,d+1), (a,b))
                | otherwise = ((a+1,b), (a,b))

decScope :: Scopes -> Scopes
decScope ((a,b), (c,d)) = ((a-1,b), (a,b))

getValue :: CurrScope -> ScopeData -> Maybe [VarType]
getValue currScope scopeData = Map.lookup currScope scopeData

printElaborator :: TypeChecker -> TypeChecker
printElaborator (Right varScopes@(VarScopes scopes scopeData)) = error $ show varScopes

debug :: TypeChecker
debug = printElaborator $ elaborate steasy