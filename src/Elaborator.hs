module Elaborator where

import Parser
import Text.Parsec.Pos
import Data.Maybe
import Data.Typeable
import Data.List (find)
import Data.Map (Map)
import Control.Exception
import Test.QuickCheck.All
import qualified Data.Map as Map

elaborate :: Script -> Script
elaborate script
    | getErrorSize result == 0 = script
    | otherwise = error $ generateErrorMessages result
    where result = tryElaborate script

tryElaborate :: Script -> TypeChecker
tryElaborate script = check script initTypeChecker

-----------------------------------------------------------------------------
-- type checking
-----------------------------------------------------------------------------

type Current    = (Int, Int)
type Previous   = (Int, Int)
type Scope      = (Current, Previous)

type ScopePath  = Map Current Previous
type ScopeMap   = (Scope, ScopePath)

type HasValue   = Bool
type Global     = Bool
type Args       = Int
type VarData    = (VarName, DataType, HasValue, Global, Args)
type ScopeVars  = Map Current [VarData]

data Context 
    = Context ScopeMap ScopeVars 
    deriving Show

data TypeChecker 
    = TypeChecker [Error] Context
    deriving Show

data Error 
    = InvalidType       VarData DataType    -- applying operation to an invalid type
    | InvalidExprType   DataType DataType   
    | DupDecl           VarName             -- duplicate entity declaration
    | MissingDecl       VarName             -- calling non-existent entity
    | NotAssigned       VarName             -- using variable without value
    | GlobalDecl        VarName             -- creating global variable not in the main scope
    | NoReturn          VarData             -- function decl. without return
    | ComingSoon
    deriving Show

basicFunctions :: [FunName]
basicFunctions = ["set_process_id", "print_str", "print", "lock", "unlock", "error"]

initTypeChecker :: TypeChecker
initTypeChecker = TypeChecker [] $ Context (((0,0), (-1,0)), (Map.insert (0,0) (-1,0) Map.empty)) Map.empty

-- builds a tree of scopes of a script and stores it as a (key, value) pair
incScope :: TypeChecker -> TypeChecker
incScope (TypeChecker errors (Context ((old@(a,b), (c,d)), scopePath) scopeVars))
    = case (Map.lookup (a+1,b) scopePath) of
        Nothing -> (TypeChecker errors (Context ((new, old), Map.insert new old scopePath) scopeVars))
        Just _  -> (TypeChecker errors (Context ((newAlt, old), Map.insert newAlt old scopePath) scopeVars))
        where new       = (a+1,b)
              newAlt    = (a+1,d+1)

-- go one step back in the scope tree
decScope :: TypeChecker -> TypeChecker
decScope typeChecker@(TypeChecker errors (Context (((0,0), (-1,0)), scopePath) scopeVars)) = typeChecker
decScope (TypeChecker errors (Context ((current, previous), scopePath) scopeVars))
                = (TypeChecker errors (Context ((fetched, current), scopePath) scopeVars))
                where fetched = fromJust $ Map.lookup current scopePath

-- returns an array type
getArrayType :: DataType -> DataType
getArrayType (ArrType dataType _) = dataType
getArrayType StrType = CharType

-- returns variables of the given scope
getValue :: Current -> ScopeVars -> Maybe [VarData]
getValue current scopeVars = Map.lookup current scopeVars

-- returns number of the errors in Type Checker
getErrorSize :: TypeChecker -> Int
getErrorSize (TypeChecker errors _) = length errors

-- determines the type of the variable
getType :: Value -> DataType
getType (Bool _) = BoolType
getType (Char _) = CharType
getType (Int _) = IntType
getType (Arr vals)
    | isText vals = StrType
    | checkArrayType vals = ArrType (getType (head vals)) (fromIntegral (length vals))
    | otherwise = error "Array contains different data types!"

checkArrayType :: [Value] -> Bool
checkArrayType [] = False
checkArrayType [x] = True
checkArrayType (x:x':xs)
    | (getType x == getType x') = checkArrayType (x':xs)
    | otherwise = False

findScopeVars :: VarName -> TypeChecker -> Maybe (Current, [VarData])
findScopeVars varName (TypeChecker _ (Context ((current@(0,0), (-1,0)), _) scopeVars))
    | isJust types      == False = Nothing
    | otherwise                  = Just (current, fromJust types)
    where types   = getValue current scopeVars
findScopeVars varName typeChecker@(TypeChecker _ (Context ((current, _), _) scopeVars)) 
    | isJust types      == False = findScopeVars varName (decScope typeChecker)
    | isJust varType == False    = findScopeVars varName (decScope typeChecker)
    | otherwise                  = Just (current, fromJust types)
    where types   = getValue current scopeVars
          varType = find (\(x, _, _, _, _) -> x == varName) (fromJust types)

-- traverses TypeChecker to find a variable in (wrapping) scopes
findVar :: VarName -> TypeChecker -> Maybe VarData
findVar varName typeChecker
    = case (findScopeVars varName typeChecker) of
        Nothing -> Nothing
        Just (_, scopeVars) -> find (\(x, _, _, _, _) -> x == varName) scopeVars

updateScopeVars :: VarName -> TypeChecker -> TypeChecker
updateScopeVars varName typeChecker@(TypeChecker errors (Context scopeMap scopeVars))
    = case (findScopeVars varName typeChecker) of
        Nothing -> typeChecker
        Just (current, varDatas) -> TypeChecker errors $ Context scopeMap $ Map.insert current (changeVar varName varDatas) scopeVars

changeVar :: VarName -> [VarData] -> [VarData]
changeVar _ [] = []
changeVar changeName (varDef@(varName, dataType, _, isGlobal, args):xs)
    | (changeName == varName) = (varName, dataType, True, isGlobal, args) : changeVar changeName xs
    | otherwise               = varDef : changeVar changeName xs

-- checks that the variable was declared in a current scope
isDeclared :: VarData -> TypeChecker -> Bool
isDeclared (varName, _, _, _, _) typeChecker@(TypeChecker _ (Context ((current, _), _) scopeVars)) 
    | isJust types      == False = False
    | isJust varType    == False = False
    | otherwise                  = True
    where   types   = getValue current scopeVars
            varType = find (\(x, _, _, _, _) -> x == varName) (fromJust types)

-- adds variable to the Context if it wasn't declared before in the same scope
addVar :: VarData -> TypeChecker -> TypeChecker
addVar varData@(varName, _, _, _, _) typeChecker@(TypeChecker errors (Context scopeMap scopeVars))
    = case (isDeclared varData typeChecker) of
        True    -> addError (DupDecl varName) typeChecker
        False   -> TypeChecker errors $ Context scopeMap $ Map.insertWith (++) (fst (fst scopeMap)) [varData] scopeVars

-- adds all function arguments to the scope
addArgs :: [VarDef] -> TypeChecker -> TypeChecker
addArgs [] typeChecker = typeChecker
addArgs ((varName, dataType):xs) typeChecker = addArgs xs $ addVar (varName, dataType, True, False, -1) typeChecker

-- adds an error to Type Checker
addError :: Error -> TypeChecker -> TypeChecker
addError error (TypeChecker errors context) 
    = TypeChecker (error : errors) context

-- generates error message with multiple errors found
generateErrorMessages :: TypeChecker -> String
generateErrorMessages (TypeChecker [] _) = []
generateErrorMessages (TypeChecker ((InvalidType (varName, dataType, _, _, _) exprDataType):xs) context) 
    = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("InvalidType Error: " ++ show varName ++ " is type of " ++ show dataType ++ " but given " ++ show exprDataType)
generateErrorMessages (TypeChecker ((InvalidExprType expectedType exprDataType):xs) context) 
    = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("InvalidExprType Error: expression type expected " ++ show expectedType ++ " but given " ++ show exprDataType)
generateErrorMessages (TypeChecker ((DupDecl varName):xs) context) 
    = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("DupDecl Error: " ++ show varName ++ " is already exists in the current scope")
generateErrorMessages (TypeChecker ((MissingDecl varName):xs) context) 
    = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("MissingDecl Error: " ++ show varName ++ " is used but wasn't declared")
generateErrorMessages (TypeChecker ((NotAssigned varName):xs) context) 
    = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("MissingDecl Error: " ++ show varName ++ " was used but has no value")
generateErrorMessages (TypeChecker ((GlobalDecl varName):xs) context) 
    = generateErrorMessages (TypeChecker xs context) ++ "\n" ++ ("GlobalDecl Error: " ++ " global variable " ++ show varName ++ " can be declared only in the main scope")

-----------------------------------------------------------------------------
-- type rules
-----------------------------------------------------------------------------

-- traverses the expression inside a statement
checkAction :: Expr -> TypeChecker -> (DataType, TypeChecker)
checkAction (Fixed (Arr values)) typeChecker = (getType (Arr values), typeChecker) 
checkAction (Fixed value) typeChecker = (getType value, typeChecker)

checkAction (Var varName) typeChecker 
    = case (findVar varName typeChecker) of
        Nothing                                 -> (IntType, addError (MissingDecl varName) typeChecker)
        Just (varName, dataType, True, _, _)    -> (dataType, typeChecker)
        Just (varName, dataType, False, _, _)   -> (IntType, addError (NotAssigned varName) typeChecker)

checkAction expr typeChecker 
    = case (expr) of
        (FunCall funName exprs) -> checkBasicFuns funName exprs typeChecker
        (Ternary ifExpr thenExpr elseExpr) -> checkTernary ifExpr thenExpr elseExpr typeChecker
        (ArrAccess varName index) -> checkArrOp varName index typeChecker
        (Both left right)   -> checkTypeOp BoolType left right typeChecker
        (OneOf left right)  -> checkTypeOp BoolType left right typeChecker
        (Eq left right)     -> checkOp left right typeChecker
        (NotEq left right)  -> checkOp left right typeChecker
        (MoreEq left right) -> checkOp left right typeChecker
        (LessEq left right) -> checkOp left right typeChecker
        (More left right)   -> checkOp left right typeChecker
        (Less left right)   -> checkOp left right typeChecker
        (Add left right)    -> checkTypeOp IntType left right typeChecker
        (Sub left right)    -> checkTypeOp IntType left right typeChecker
        (Mult left right)   -> checkTypeOp IntType left right typeChecker
        (Neg expr)          -> checkAction expr typeChecker

checkTernary :: Expr -> Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkTernary ifExpr thenExpr elseExpr typeChecker
    | (ifExprDataType == BoolType) == False = error $ "Ternary operator should check Bool but given " ++ show ifExprDataType
    | otherwise = (elseExprDataType, elseExprTypeChecker)
    where (ifExprDataType, ifExprTypeChecker) = checkAction ifExpr typeChecker
          (_, thenExprTypeChecker) = checkAction thenExpr ifExprTypeChecker
          (elseExprDataType, elseExprTypeChecker) = checkAction elseExpr thenExprTypeChecker

checkBasicFuns :: FunName -> [Expr] -> TypeChecker -> (DataType, TypeChecker)
checkBasicFuns funName exprs typeChecker
    | (funName == "set_process_id") = (IntType, updateScopeVars varName typeChecker)
    | funName `elem` basicFunctions = (IntType, typeChecker)
    | otherwise = checkFunCall funName exprs typeChecker
    where (Var varName) = head exprs

checkFunCall :: FunName -> [Expr] -> TypeChecker -> (DataType, TypeChecker)
checkFunCall funName exprs typeChecker
    | isJust (findVar funName typeChecker) == False = error $ "MissingDecl Error: function " ++ funName ++ " wasn't defined before usage"
    | (args == length exprs) == False = error $ "InvalidArgsNumber Error: function " ++ funName ++ " has " ++ show args ++ " argument but " ++ show (length exprs) ++ " were given"
    | otherwise = (dataType, typeChecker)
    where (_, dataType, _, _, args) = fromJust $ findVar funName typeChecker

checkOp :: Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkOp left right typeChecker 
    | (leftDataType == rightDataType) == False  = error $ "\nInvalidType Error: different types used in operation: " ++ show leftDataType ++ " and " ++ show rightDataType
    | otherwise                                 = (BoolType, rightTypeChecker)
    where   (leftDataType, leftTypeChecker)     = checkAction left typeChecker
            (rightDataType, rightTypeChecker)   = checkAction right leftTypeChecker

checkTypeOp :: DataType -> Expr -> Expr -> TypeChecker -> (DataType, TypeChecker)
checkTypeOp dataType left right typeChecker
    | (leftDataType == rightDataType) == False  = error $ "\nInvalidType Error: different types used in operation: " ++ show leftDataType ++ " and " ++ show rightDataType
    | (leftDataType == dataType)      == False  = error $ "\nInvalidType Error: expected to return " ++ show dataType ++ " but returned " ++ show leftDataType
    | otherwise                                 = (dataType, rightTypeChecker)
    where   (leftDataType, leftTypeChecker)     = checkAction left typeChecker
            (rightDataType, rightTypeChecker)   = checkAction right leftTypeChecker

checkArrOp :: VarName -> Integer -> TypeChecker -> (DataType, TypeChecker)
checkArrOp varName index typeChecker
    = case (findVar varName typeChecker) of
        Nothing -> error $ "MissingDecl Error: array " ++ varName ++ " wasn't declared before usage"
        Just (_, dataType, _, _, _) -> (getArrayType dataType, typeChecker)

-- checks that the value has a correct data type and adds it to the Context
checkVarDecl :: Global -> VarDef -> Maybe Expr -> TypeChecker -> TypeChecker
checkVarDecl global (varName, varType) Nothing typeChecker = addVar (varName, varType, False, global, -1) typeChecker
checkVarDecl global (varName, dataType) (Just expr) typeChecker
    | (dataType /= exprDataType)            = addError (InvalidType (varName, dataType, True, global, -1) exprDataType) typeChecker
    | otherwise                             = addVar (varName, dataType, True, global, -1) exprTypeChecker
    where (exprDataType, exprTypeChecker)   = checkAction expr typeChecker

-- checks that the global variable was initialized in the main scope
checkGlVarDecl :: VarDef -> Maybe Expr -> TypeChecker -> TypeChecker
checkGlVarDecl varDef@(varName, _) maybeExpr typeChecker@(TypeChecker _ (Context ((current, _), _) _))
    | current == (0,0)  = checkVarDecl True varDef maybeExpr typeChecker
    | otherwise         = addError (GlobalDecl varName) typeChecker

-- checks that the variable exists in the (wrapping) scope and has a correct data type
checkVarAssign :: VarName -> Expr -> TypeChecker -> TypeChecker
checkVarAssign varName expr typeChecker
    | isJust (findVar varName typeChecker) == False = addError (MissingDecl varName) typeChecker
    | (dataType /= exprDataType)                    = addError (InvalidType (newVarName, dataType, hasValue, isGlobal, args) exprDataType) typeChecker
    | otherwise                                     = updateScopeVars varName exprTypeChecker
    where   (newVarName, dataType, hasValue, isGlobal, args)    = fromJust $ findVar varName typeChecker 
            (exprDataType, exprTypeChecker)     = checkAction expr typeChecker

checkArrInsert :: VarName -> Integer -> Expr -> TypeChecker -> TypeChecker
checkArrInsert varName index expr typeChecker
    | isJust (findVar varName typeChecker) == False = addError (MissingDecl varName) typeChecker
    | (getArrayType dataType /= exprDataType)       = addError (InvalidType (newVarName, dataType, hasValue, isGlobal, args) exprDataType) typeChecker
    | otherwise                                     = exprTypeChecker
    where   (newVarName, dataType, hasValue, isGlobal, args)                   = fromJust $ findVar varName typeChecker
            (exprDataType, exprTypeChecker) = checkAction expr typeChecker
 
checkFunDef :: FunName -> ArgsDef -> Maybe DataType -> Script -> TypeChecker -> TypeChecker
checkFunDef funName argsDef maybeDataType script typeChecker
    | isJust maybeDataType == False = checkNoFunReturn script argsDefTypeChecker
    | otherwise = scriptTypeChecker
    where funNameTypeChecker = addVar (funName, IntType, False, False, (length argsDef)) typeChecker
          argsDefTypeChecker = addArgs argsDef $ incScope funNameTypeChecker
          scriptTypeChecker = check script argsDefTypeChecker

getLastScriptElement :: Script -> Statement
getLastScriptElement [] = (InScope [])
getLastScriptElement script = last script

checkNoFunReturn :: Script -> TypeChecker -> TypeChecker
checkNoFunReturn script typeChecker
    = case (getLastScriptElement script) of
        (ReturnVal expr) -> error $ "NoReturn Error: return statement is not needed"
        _ -> check script typeChecker

checkReturnVal :: Expr -> TypeChecker -> TypeChecker
checkReturnVal expr typeChecker = decScope returnTypeChecker
    where (dataType, returnTypeChecker) = checkAction expr typeChecker

checkForLoop :: VarDef -> LoopIter -> Script -> TypeChecker -> TypeChecker
checkForLoop (varName, IntType) (IterRange left right) script typeChecker = scriptTypeChecker
    where   varDefTypeChecker           = addVar (varName, IntType, True, False, -1) $ incScope typeChecker
            (_, iterRangeTypeChecker)   = checkTypeOp IntType left right varDefTypeChecker
            scriptTypeChecker           = check script varDefTypeChecker
checkForLoop (varName, dataType) _ _ typeChecker    = addError (InvalidType (varName, dataType, True, False, -1) IntType) typeChecker

checkWhileLoop :: Expr -> Script -> TypeChecker -> TypeChecker
checkWhileLoop expr script typeChecker  = check script $ incScope whileTypeChecker
    where (_, whileTypeChecker)         = checkAction expr typeChecker

checkCondition :: Expr -> Script -> Maybe Script -> TypeChecker -> TypeChecker
checkCondition expr ifScript elseScript typeChecker
    | (ifType == BoolType) == False = addError (InvalidExprType BoolType ifType) typeChecker
    | isJust elseScript == False = check ifScript $ incScope ifTypeChecker
    | otherwise = check (fromJust elseScript) $ incScope $ check ifScript (incScope ifTypeChecker)
    where (ifType, ifTypeChecker) = checkAction expr typeChecker

-- checks the correctness of the written program (types and scopes)
check :: Script -> TypeChecker -> TypeChecker
check [] typeChecker = decScope typeChecker

check ((VarDecl varType maybeExpr):xs) typeChecker 
    = check xs $ checkVarDecl False varType maybeExpr typeChecker

check ((GlVarDecl varType maybeExpr):xs) typeChecker 
    = check xs $ checkGlVarDecl varType maybeExpr typeChecker

check ((VarAssign varName expr):xs) typeChecker
    = check xs $ checkVarAssign varName expr typeChecker

check ((ArrInsert varName index expr):xs) typeChecker 
    = check xs $ checkArrInsert varName index expr typeChecker

check ((FunDef funName argsDef maybeDataType script):xs) typeChecker 
    = check xs $ checkFunDef funName argsDef maybeDataType script typeChecker

check ((ForLoop varType loopIter script):xs) typeChecker 
    = check xs $ checkForLoop varType loopIter script typeChecker

check ((WhileLoop expr script):xs) typeChecker 
    = check xs $ checkWhileLoop expr script typeChecker

check ((Condition expr script maybeScript):xs) typeChecker 
    = check xs $ checkCondition expr script maybeScript typeChecker

check ((Parallel num script):xs) typeChecker 
    = check xs $ check script $ incScope typeChecker

check ((InScope script):xs) typeChecker 
    = check xs $ check script $ incScope typeChecker

check ((ReturnVal expr):xs) typeChecker 
    = checkReturnVal expr typeChecker

check ((Action expr):xs) typeChecker 
    = check xs $ snd $ checkAction expr typeChecker