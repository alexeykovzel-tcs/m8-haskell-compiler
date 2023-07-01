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

-- Merges two type checkers into one
-- merge :: TypeChecker -> TypeChecker -> TypeChecker

-- Adds error to the list of errors
addError :: Error -> TypeChecker -> TypeChecker
addError error (Left errors) = Left $ error : errors

-- Prepares a scope to be used in search
prepareScope :: CurrScope -> CurrScope
prepareScope (a,b) = (a,0)

-- Traverses TypeChecker to find a variable in (wrapping) scopes
findVar :: VarName -> TypeChecker -> Maybe VarDef
findVar _ (Right (VarScopes ((-1, _), _) scopeData)) = Nothing
findVar vName typeChecker@(Right (VarScopes (currScope, _) scopeData)) 
                    | isJust types == False = findVar vName (decScope typeChecker)
                    | isJust varType == False = findVar vName (decScope typeChecker)
                    | otherwise = varType
                    where types = getValue (prepareScope currScope) scopeData
                          varType = find (\(x, _) -> x == vName) (fromJust types)

-- Adds variable to VarScopes
addVar :: VarDef -> TypeChecker -> TypeChecker
addVar varType (Right (VarScopes scopes scopeData)) 
                    = Right $ VarScopes scopes (Map.insertWith (++) (fst scopes) [varType] scopeData)

-- Determines the type of the variable
getType :: Value -> DataType
getType (Text _) = StrType
getType (Bool _) = BoolType
getType (Int _) = IntType

-- Traverses the expression inside a statement
checkAction :: Expr -> TypeChecker -> (DataType, TypeChecker)
checkAction (Fixed value) typeChecker = (getType value, typeChecker)
checkAction (Var varName) typeChecker = 
                case (findVar varName typeChecker) of
                    Nothing                   -> error "MissingDecl Action"
                    Just (varName, dataType)  -> (dataType, typeChecker)
checkAction expr typeChecker =
                case (expr) of
                    (Both left right)   -> checkBoolOp left right typeChecker
                    (OneOf left right)  -> checkBoolOp left right typeChecker
                    (Eq left right)     -> checkOp left right typeChecker
                    (MoreEq left right) -> checkOp left right typeChecker
                    (LessEq left right) -> checkOp left right typeChecker
                    (More left right)   -> checkOp left right typeChecker
                    (Less left right)   -> checkOp left right typeChecker
                    (Add left right)    -> checkIntOp left right typeChecker
                    (Sub left right)    -> checkIntOp left right typeChecker
                    (Mult left right)   -> checkIntOp left right typeChecker

checkIntOp :: Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkIntOp left right typeChecker = 
                case (leftDataType == rightDataType && leftDataType == IntType) of
                    False -> error "InvalidType IntOp"
                    True  -> (IntType, rightTypeChecker)
                where (leftDataType, leftTypeChecker)   = checkAction left typeChecker
                      (rightDataType, rightTypeChecker)  = checkAction right leftTypeChecker

checkOp :: Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkOp left right typeChecker = 
                case (leftDataType == rightDataType) of
                    False -> error "InvalidType Op"
                    True  -> (BoolType, rightTypeChecker)
                where (leftDataType, leftTypeChecker)   = checkAction left typeChecker
                      (rightDataType, rightTypeChecker)  = checkAction right leftTypeChecker

checkBoolOp :: Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkBoolOp left right typeChecker = 
                case (leftDataType == rightDataType && leftDataType == BoolType) of
                    False -> error "InvalidType BoolOp"
                    True  -> (BoolType, rightTypeChecker)
                where (leftDataType, leftTypeChecker)   = checkAction left typeChecker
                      (rightDataType, rightTypeChecker)  = checkAction right leftTypeChecker

-- Checks that the assigned value is compatible with the type of variable
checkVarDecl :: VarDef -> Maybe Expr -> TypeChecker -> TypeChecker
checkVarDecl (varName, dataType) Nothing typeChecker = addVar (varName, dataType) typeChecker
checkVarDecl (varName, dataType) (Just expr) typeChecker
                | (dataType /= exprDataType) = error "InvalidType VarDecl"
                | otherwise                  = addVar (varName, dataType) exprTypeChecker
                where (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkVarAssign :: VarName -> Expr -> TypeChecker -> TypeChecker
checkVarAssign varName expr typeChecker
                | isJust (findVar varName typeChecker) == False = error "MissingDecl VarAssign"
                | (dataType /= exprDataType)                    = error "InvalidType VarAssign"
                | otherwise                                     = exprTypeChecker
                where (_, dataType)                   = fromJust $ findVar varName typeChecker 
                      (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkCondition :: Expr -> Script -> Maybe Script -> TypeChecker -> TypeChecker
checkCondition expr ifScript elseScript typeChecker =
                case (elseScript) of
                    Nothing -> check ifScript (incScope ifTypeChecker)
                    Just script -> check script $ incScope $ check ifScript (incScope ifTypeChecker)
                where (_, ifTypeChecker) = checkAction expr typeChecker

-- Checks the correctness of the written program
check :: Script -> TypeChecker -> TypeChecker
check [] typeChecker = decScope typeChecker
check ((VarDecl varType maybeExpr):xs) typeChecker = check xs $ checkVarDecl varType maybeExpr typeChecker
check ((VarAssign varName expr):xs) typeChecker = check xs $ checkVarAssign varName expr typeChecker
check ((Condition expr script maybeScript):xs) typeChecker 
                = check xs $ checkCondition expr script maybeScript typeChecker
check ((Action expr):xs) typeChecker = check xs $ snd $ checkAction expr typeChecker

-- Generates error message with multiple errors found
generateErrorMessage :: TypeChecker -> String
generateErrorMessage (Left errors) = error "Multiple Errors!"

elaborate :: Script -> TypeChecker
elaborate script = check script initTypeChecker

-----------------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------------

steasy :: Script
steasy = tryParse script "let x: Int = 5; let y: Int = 0; if x < y { } else { x = true; }"

initScope :: Scopes
initScope = ((0,0), (0,0))

initTypeChecker :: TypeChecker
initTypeChecker = Right $ VarScopes initScope Map.empty

incScope :: TypeChecker -> TypeChecker
incScope (Right (VarScopes ((a,b), (c,d)) scopeData))
                | (a+1) == c = (Right (VarScopes ((a+1,d+1), (a,b)) scopeData))
                | otherwise = (Right (VarScopes ((a+1,b), (a,b)) scopeData))

decScope :: TypeChecker -> TypeChecker
decScope (Right (VarScopes ((a,b), (c,d)) scopeData)) = (Right (VarScopes ((a-1,b), (a,b)) scopeData))

getValue :: CurrScope -> ScopeData -> Maybe [VarDef]
getValue currScope scopeData = Map.lookup currScope scopeData

printElaborator :: TypeChecker -> TypeChecker
printElaborator (Right varScopes@(VarScopes scopes scopeData)) = error $ show varScopes

debug :: TypeChecker
debug = printElaborator $ elaborate steasy