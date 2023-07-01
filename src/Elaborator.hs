module Elaborator where

import Parser
import Text.Parsec.Pos
import Data.Maybe
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map

-- -----------------------------------------------------------------------------
-- -- Type Checking
-- -----------------------------------------------------------------------------

type CurrScope = (Int, Int)
type PrevScope = (Int, Int)
type Scopes = (CurrScope, PrevScope)

-- type VarType = (VarName, DataType)
type ScopeData = Map CurrScope [VarDef]

type TypeChecker = Either [Error] VarScopes

data VarScopes = VarScopes Scopes ScopeData deriving Show

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
findVar :: VarName -> TypeChecker -> Maybe VarDef
findVar _ (Right (VarScopes ((-1, _), _) scopeData)) = Nothing
findVar vName (Right (VarScopes scope@(currScope, _) scopeData)) 
                    | isJust varType = varType
                    | otherwise      = findVar vName (Right (VarScopes (decScope scope) scopeData))
                    where types = fromJust $ getValue currScope scopeData
                          varType = find (\(x, _) -> x == vName) types

-- Adds variable to VarScopes
addVar :: VarDef -> TypeChecker -> TypeChecker
addVar varType (Right (VarScopes scopes scopeData)) 
                    = Right $ VarScopes scopes (Map.insertWith (++) (fst scopes) [varType] scopeData)

-- Determines the type of the variable
getType :: Value -> DataType
getType (Text _) = StrType
getType (Bool _) = BoolType
getType (Int _) = IntType

checkAction :: Expr -> TypeChecker -> (DataType, TypeChecker)
checkAction (Fixed value) typeChecker = (getType value, typeChecker)
checkAction (Var varName) typeChecker = 
                case (findVar varName typeChecker) of
                    Nothing                   -> error "MissingDecl"
                    Just (varName, dataType)  -> (dataType, typeChecker)
checkAction (Mult left right) typeChecker =
                case (leftDataType == rightDataType && leftDataType == IntType) of
                    False -> error "InvalidType"
                    True  -> (IntType, typeChecker)
                where (leftDataType, _)   = checkAction left typeChecker
                      (rightDataType, _)  = checkAction right typeChecker

-- Checks that the assigned value is compatible with the type of variable
checkVarDecl :: VarDef -> Maybe Expr -> TypeChecker -> TypeChecker
checkVarDecl (varName, dataType) Nothing typeChecker = addVar (varName, dataType) typeChecker
checkVarDecl (varName, dataType) (Just expr) typeChecker
                | (dataType /= exprDataType) = error "InvalidType"
                | otherwise                  = addVar (varName, dataType) exprTypeChecker
                where (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkVarAssign :: VarName -> Expr -> TypeChecker -> TypeChecker
checkVarAssign varName expr typeChecker
                | isJust (findVar varName typeChecker) == False = error "MissingDecl"
                | (dataType /= exprDataType)                    = error "InvalidType"
                | otherwise                                     = exprTypeChecker
                where (_, dataType)                   = fromJust $ findVar varName typeChecker 
                      (exprDataType, exprTypeChecker) = checkAction expr typeChecker

-- Checks the correctness of the written program
check :: Script -> TypeChecker -> TypeChecker
check [] (Right (VarScopes scopes scopeData)) = (Right (VarScopes (decScope scopes) scopeData))
check ((VarDecl varType maybeExpr):xs) typeChecker = check xs $ checkVarDecl varType maybeExpr typeChecker
check ((VarAssign varName expr):xs) typeChecker = check xs $ checkVarAssign varName expr typeChecker
check ((Action expr):xs) typeChecker = check xs newTypeChecker
                where (_, newTypeChecker) = checkAction expr typeChecker
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

-- -----------------------------------------------------------------------------
-- -- Utilities
-- -----------------------------------------------------------------------------

steasy :: Script
steasy = tryParse script "let x: Int = true * 5;"

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

getValue :: CurrScope -> ScopeData -> Maybe [VarDef]
getValue currScope scopeData = Map.lookup currScope scopeData

printElaborator :: TypeChecker -> TypeChecker
printElaborator (Right varScopes@(VarScopes scopes scopeData)) = error $ show varScopes

debug :: TypeChecker
debug = printElaborator $ elaborate steasy