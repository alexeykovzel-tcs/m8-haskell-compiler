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
- Fix scope back traverse (add path map)
- Add support for in scopeMap
- Add support for functions
- Add multiple error message
- Add tests
- Clean the code
- Check that var has value
-}

type Current = (Int, Int)
type Previous = (Int, Int)
type Scope = (Current, Previous)

type ScopePath = Map Current Previous
type ScopeMap = (Scope, ScopePath)
type ScopeVars = Map Current [VarDef]

data Context = Context ScopeMap ScopeVars deriving Show
type TypeChecker = Either [Error] Context

data Error 
    = InvalidType   SourcePos String    -- applying operation to an invalid type
    | DupDecl       SourcePos String    -- duplicate entity declaration
    | MissingDecl   SourcePos String    -- calling non-existent entity
    | NotAssigned   SourcePos String    -- using variable without value
    | NoReturn      SourcePos String    -- function decl. without return
    deriving Show

getArrayType :: DataType -> DataType
getArrayType (ArrType dataType _) = dataType

-- Adds error to the list of errors
addError :: Error -> TypeChecker -> TypeChecker
addError error (Left errors) = Left $ error : errors

-- Traverses TypeChecker to find a variable in (wrapping) scopeMap
findVar :: VarName -> TypeChecker -> Maybe VarDef
findVar _ (Right (Context (((-1,0), _), _) scopeVars)) = Nothing
findVar varName typeChecker@(Right (Context ((current, _), _) scopeVars)) 
                | isJust types == False = findVar varName (decScope typeChecker)
                | isJust varType == False = findVar varName (decScope typeChecker)
                | otherwise = varType
                where types = getValue current scopeVars
                      varType = find (\(x, _) -> x == varName) (fromJust types)

isDeclared :: VarDef -> TypeChecker -> Bool
isDeclared (varName, _) typeChecker@(Right (Context ((current, _), _) scopeVars)) 
                | isJust types == False = False
                | isJust varType == False = False
                | otherwise = True
                where types = getValue current scopeVars
                      varType = find (\(x, _) -> x == varName) (fromJust types)

-- Adds variable to Context if it wasn't declared before in the same scope
addVar :: VarDef -> TypeChecker -> TypeChecker
addVar varDef typeChecker@(Right (Context scopeMap scopeVars)) =
                case (isDeclared varDef typeChecker) of
                    True    -> error $ "DupDecl AddVar " ++ show varDef
                    False   -> Right $ Context scopeMap $ Map.insertWith (++) (fst (fst scopeMap)) [varDef] scopeVars

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
                | (dataType /= exprDataType) = error "MissingDecl VarDecl"
                | otherwise                  = addVar (varName, dataType) exprTypeChecker
                where (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkVarAssign :: VarName -> Expr -> TypeChecker -> TypeChecker
checkVarAssign varName expr typeChecker
                | isJust (findVar varName typeChecker) == False = error "MissingDecl VarAssign"
                | (dataType /= exprDataType)                    = error "InvalidType VarAssign"
                | otherwise                                     = exprTypeChecker
                where (_, dataType)                   = fromJust $ findVar varName typeChecker 
                      (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkArrInsert :: VarName -> Integer -> Expr -> TypeChecker -> TypeChecker
checkArrInsert varName index expr typeChecker
                | isJust (findVar varName typeChecker) == False = error "MissingDecl ArrInsert"
                | (getArrayType dataType /= exprDataType) = error "InvalidType ArrInsert"
                | otherwise = exprTypeChecker
                where (_, dataType) = fromJust $ findVar varName typeChecker
                      (exprDataType, exprTypeChecker) = checkAction expr typeChecker

checkForLoop :: VarDef -> LoopIter -> Script -> TypeChecker -> TypeChecker
checkForLoop varType@(_, IntType) (IterRange left right) script typeChecker = scriptTypeChecker
                where varDefTypeChecker = addVar varType $ incScope typeChecker
                      (_, iterRangeTypeChecker) = checkIntOp left right varDefTypeChecker
                      scriptTypeChecker = check script iterRangeTypeChecker
checkForLoop (_, _) _ _ _ = error "InvalidType ForLoop"

checkWhileLoop :: Expr -> Script -> TypeChecker -> TypeChecker
checkWhileLoop expr script typeChecker
                = check script $ incScope whileTypeChecker
                where (_, whileTypeChecker) = checkAction expr typeChecker

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
check ((ArrInsert varName index expr):xs) typeChecker = check xs $ checkArrInsert varName index expr typeChecker
check ((ForLoop varType loopIter script):xs) typeChecker 
                = check xs $ checkForLoop varType loopIter script typeChecker
check ((WhileLoop expr script):xs) typeChecker = check xs $ checkWhileLoop expr script typeChecker
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
steasy = tryParse script "let x: Int = 5; let y: Int = 0; \
                         \if x < y { let x: Int; while x < 3 { let z: Bool; } } else { let y: Int; } \
                         \let z: Bool = true;"

initTypeChecker :: TypeChecker
initTypeChecker = Right $ Context (((0,0), (-1,0)), (Map.insert (0,0) (-1,0) Map.empty)) Map.empty

-- FIX -> WRONG INCREMENT VALUE
incScope :: TypeChecker -> TypeChecker
incScope (Right (Context ((old@(a,b), (c,d)), scopePath) scopeVars))
                | (a+1) == c = (Right (Context ((new, old), Map.insert new old scopePath) scopeVars))
                | otherwise = (Right (Context ((newAlt, old), Map.insert newAlt old scopePath) scopeVars))
                where new = (a+1, d+1)
                      newAlt = (a+1,b)

decScope :: TypeChecker -> TypeChecker
decScope typeChecker@(Right (Context (((0,0), previous), scopePath) scopeVars)) = typeChecker
decScope (Right (Context ((_, previous), scopePath) scopeVars))
                = (Right (Context ((previous, fetched), scopePath) scopeVars))
                where fetched = fromJust $ Map.lookup previous scopePath

getValue :: Current -> ScopeVars -> Maybe [VarDef]
getValue current scopeVars = Map.lookup current scopeVars

printTypeChecker :: TypeChecker -> TypeChecker
printTypeChecker (Left errors) = error $ show errors
printTypeChecker (Right varScopes@(Context scopeMap scopeVars)) = error $ show varScopes

debug :: TypeChecker
debug = printTypeChecker $ elaborate steasy