module Elaborator where

import Parser
import Text.Parsec.Pos
import Data.Maybe
import Data.Typeable
import Data.List (find)
import Data.Map (Map)
import Control.Exception
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- Type Checking
-----------------------------------------------------------------------------

{-
TODO:
- Add tests
- Clean the code
- Check that var has value
- Add support for functions
-}

type Current = (Int, Int)
type Previous = (Int, Int)
type Scope = (Current, Previous)

type ScopePath = Map Current Previous
type ScopeMap = (Scope, ScopePath)
type ScopeVars = Map Current [VarDef]

data Context 
    = Context ScopeMap ScopeVars 
    deriving Show

data TypeChecker 
    = TypeChecker [Error] Context
    deriving Show

data Error 
    = InvalidType   VarDef    -- applying operation to an invalid type
    | DupDecl       VarName    -- duplicate entity declaration
    | MissingDecl   VarName    -- calling non-existent entity
    | NotAssigned   VarName    -- using variable without value
    | NoReturn      VarDef    -- function decl. without return
    deriving Show

getArrayType :: DataType -> DataType
getArrayType (ArrType dataType _) = dataType

-- Determines the type of the variable
getType :: Value -> DataType
getType (Bool _) = BoolType
getType (Char _) = CharType
getType (Int _) = IntType
getType (Arr [Char _]) = StrType

-- Traverses TypeChecker to find a variable in (wrapping) scopeMap
findVar :: VarName -> TypeChecker -> Maybe VarDef
findVar varName (TypeChecker _ (Context ((current@(0,0), (-1,0)), _) scopeVars))
                | isJust types == False = Nothing
                | isJust varType == False = Nothing
                | otherwise = varType
                where types = getValue current scopeVars
                      varType = find (\(x, _) -> x == varName) (fromJust types)
findVar varName typeChecker@(TypeChecker _ (Context ((current, _), _) scopeVars)) 
                | isJust types == False = findVar varName (decScope typeChecker)
                | isJust varType == False = findVar varName (decScope typeChecker)
                | otherwise = varType
                where types = getValue current scopeVars
                      varType = find (\(x, _) -> x == varName) (fromJust types)

isDeclared :: VarDef -> TypeChecker -> Bool
isDeclared (varName, _) typeChecker@(TypeChecker _ (Context ((current, _), _) scopeVars)) 
                | isJust types == False = False
                | isJust varType == False = False
                | otherwise = True
                where types = getValue current scopeVars
                      varType = find (\(x, _) -> x == varName) (fromJust types)

-- Adds variable to Context if it wasn't declared before in the same scope
addVar :: VarDef -> TypeChecker -> TypeChecker
addVar varDef@(varName, dataType) typeChecker@(TypeChecker errors (Context scopeMap scopeVars)) =
                case (isDeclared varDef typeChecker) of
                    True    -> addError (DupDecl varName) typeChecker
                    False   -> TypeChecker errors $ Context scopeMap $ Map.insertWith (++) (fst (fst scopeMap)) [varDef] scopeVars

addError :: Error -> TypeChecker -> TypeChecker
addError error (TypeChecker errors context) = TypeChecker (error : errors) context

-- Traverses the expression inside a statement
checkAction :: Expr -> TypeChecker -> (DataType, TypeChecker)
checkAction (Fixed value) typeChecker = (getType value, typeChecker)
checkAction (Var varName) typeChecker = 
                case (findVar varName typeChecker) of
                    Nothing                   -> (IntType, addError (MissingDecl varName) typeChecker)
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
                    False -> (IntType, rightTypeChecker)
                    True  -> (IntType, rightTypeChecker)
                where (leftDataType, leftTypeChecker)   = checkAction left typeChecker
                      (rightDataType, rightTypeChecker)  = checkAction right leftTypeChecker

checkOp :: Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkOp left right typeChecker = 
                case (leftDataType == rightDataType) of
                    False -> (BoolType, rightTypeChecker)
                    True  -> (BoolType, rightTypeChecker)
                where (leftDataType, leftTypeChecker)   = checkAction left typeChecker
                      (rightDataType, rightTypeChecker)  = checkAction right leftTypeChecker

checkBoolOp :: Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkBoolOp left right typeChecker = 
                case (leftDataType == rightDataType && leftDataType == BoolType) of
                    False -> (BoolType, rightTypeChecker)
                    True  -> (BoolType, rightTypeChecker)
                where (leftDataType, leftTypeChecker)   = checkAction left typeChecker
                      (rightDataType, rightTypeChecker)  = checkAction right leftTypeChecker

-- Checks that the assigned value is compatible with the type of variable
checkVarDecl :: VarDef -> Maybe Expr -> TypeChecker -> TypeChecker
checkVarDecl (varName, dataType) Nothing typeChecker = addVar (varName, dataType) typeChecker
checkVarDecl (varName, dataType) (Just expr) typeChecker
                | (dataType /= exprDataType) = addError (InvalidType (varName, dataType)) typeChecker
                | otherwise                  = addVar (varName, dataType) exprTypeChecker
                where (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkVarAssign :: VarName -> Expr -> TypeChecker -> TypeChecker
checkVarAssign varName expr typeChecker
                | isJust (findVar varName typeChecker) == False = addError (MissingDecl varName) typeChecker
                | (dataType /= exprDataType)                    = addError (InvalidType (varName, dataType)) typeChecker
                | otherwise                                     = exprTypeChecker
                where (_, dataType)                   = fromJust $ findVar varName typeChecker 
                      (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkArrInsert :: VarName -> Integer -> Expr -> TypeChecker -> TypeChecker
checkArrInsert varName index expr typeChecker
                | isJust (findVar varName typeChecker) == False = addError (MissingDecl varName) typeChecker
                | (getArrayType dataType /= exprDataType) = addError (InvalidType (varName, dataType)) typeChecker
                | otherwise = exprTypeChecker
                where (_, dataType) = fromJust $ findVar varName typeChecker
                      (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkForLoop :: VarDef -> LoopIter -> Script -> TypeChecker -> TypeChecker
checkForLoop varType@(_, IntType) (IterRange left right) script typeChecker = scriptTypeChecker
                where varDefTypeChecker = addVar varType $ incScope typeChecker
                      (_, iterRangeTypeChecker) = checkIntOp left right varDefTypeChecker
                      scriptTypeChecker = check script iterRangeTypeChecker
checkForLoop varType _ _ typeChecker = addError (InvalidType varType) typeChecker

checkWhileLoop :: Expr -> Script -> TypeChecker -> TypeChecker
checkWhileLoop expr script typeChecker
                = check script $ incScope whileTypeChecker
                where (_, whileTypeChecker) = checkAction expr typeChecker

checkCondition :: Expr -> Script -> Maybe Script -> TypeChecker -> TypeChecker
checkCondition expr ifScript elseScript typeChecker =
                case (elseScript) of
                    Nothing -> check ifScript $ incScope ifTypeChecker
                    Just script -> check script $ incScope $ check ifScript (incScope ifTypeChecker)
                where (_, ifTypeChecker) = checkAction expr typeChecker

-- Checks the correctness of the written program
check :: Script -> TypeChecker -> TypeChecker
check [] typeChecker = decScope typeChecker
check ((VarDecl varType maybeExpr):xs) typeChecker = check xs $ checkVarDecl varType maybeExpr typeChecker
check ((VarAssign varName expr):xs) typeChecker = check xs $ checkVarAssign varName expr typeChecker
check ((ArrInsert varName index expr):xs) typeChecker = check xs $ checkArrInsert varName index expr typeChecker
check ((ForLoop varType loopIter script):xs) typeChecker 
                = check xs $ checkForLoop varType loopIter script typeChecker
check ((WhileLoop expr script):xs) typeChecker = check xs $ checkWhileLoop expr script typeChecker
check ((Condition expr script maybeScript):xs) typeChecker 
                = check xs $ checkCondition expr script maybeScript typeChecker
check ((InScope script):xs) typeChecker = check xs $ check script $ incScope typeChecker
check ((Action expr):xs) typeChecker = check xs $ snd $ checkAction expr typeChecker

elaborate :: Script -> TypeChecker
elaborate script = check script initTypeChecker

-----------------------------------------------------------------------------
-- Error Generation
-----------------------------------------------------------------------------

-- Generates error message with multiple errors found
generateErrorMessages :: TypeChecker -> String
generateErrorMessages (TypeChecker [] _) = []
generateErrorMessages (TypeChecker ((InvalidType (varName, dataType)):xs) context) 
                = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("InvalidType Error: " ++ show varName ++ " is type of " ++ show dataType ++ " but given " ++ "CHANGE")
generateErrorMessages (TypeChecker ((DupDecl varName):xs) context) 
                = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("DupDecl Error: " ++ show varName ++ " is already exists in the current scope ")
generateErrorMessages (TypeChecker ((MissingDecl varName):xs) context) 
                = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("MissingDecl Error: " ++ show varName ++ " is used but wasn't declared ")
generateErrorMessages (TypeChecker ((NotAssigned varName):xs) context) 
                = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("MissingDecl Error: " ++ show varName ++ " was used but has no value ")

-----------------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------------

steasy :: Script
steasy = tryParse script "let x: Int = 0; let x: Int = 5; { z = 5; let x: Bool = true; }"

initTypeChecker :: TypeChecker
initTypeChecker = TypeChecker [] $ Context (((0,0), (-1,0)), (Map.insert (0,0) (-1,0) Map.empty)) Map.empty

incScope :: TypeChecker -> TypeChecker
incScope (TypeChecker errors (Context ((old@(a,b), (c,d)), scopePath) scopeVars)) =
                case (Map.lookup (a+1,b) scopePath) of
                    Nothing -> (TypeChecker errors (Context ((new, old), Map.insert new old scopePath) scopeVars))
                    Just _ -> (TypeChecker errors (Context ((newAlt, old), Map.insert newAlt old scopePath) scopeVars))
                    where new = (a+1,b)
                          newAlt = (a+1,d+1)

decScope :: TypeChecker -> TypeChecker
decScope typeChecker@(TypeChecker errors (Context (((0,0), (-1,0)), scopePath) scopeVars)) = typeChecker
decScope (TypeChecker errors (Context ((_, previous), scopePath) scopeVars))
                = (TypeChecker errors (Context ((previous, fetched), scopePath) scopeVars))
                where fetched = fromJust $ Map.lookup previous scopePath

getValue :: Current -> ScopeVars -> Maybe [VarDef]
getValue current scopeVars = Map.lookup current scopeVars

printTypeChecker :: TypeChecker -> String
printTypeChecker typeChecker@(TypeChecker [] _) = show typeChecker
printTypeChecker typeChecker@(TypeChecker errors _) = generateErrorMessages typeChecker

debug :: a
debug = error $ printTypeChecker $ elaborate steasy