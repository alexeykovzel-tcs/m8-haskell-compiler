{-# LANGUAGE FlexibleInstances #-}

module PostParser where

import Parser

postScript :: Script -> Script
postScript [] = []
postScript (x:xs) = postStmt x ++ postScript xs

postStmt :: Statement -> Script
postStmt (WhileLoop cond body)  = [WhileLoop cond (postScript body)]
postStmt (InScope body)         = [InScope (postScript body)]

postStmt (Condition cond ifBody elseBody) = 
    [Condition cond (postScript ifBody) postElse]
    where 
        postElse = case elseBody of
            Just body   -> Just $ postScript body
            Nothing     -> Nothing

-- add variable for code address to the function
postStmt (FunDef name args returnType body) = [
        VarDecl ("_f_" ++ name, IntType) Nothing,
        FunDef name args returnType (postScript body) 
    ]

-- change "for" loop to "while" as it's easier to compile
postStmt (ForLoop i@(name, _) (IterRange from to) body) = [
    InScope [
        VarDecl i (Just $ from),
        VarDecl ("_to", IntType) (Just $ to),
        WhileLoop whileCond whileBody 
    ]]
    where 
        incrI     = VarAssign name $ Add (Var name) (Fixed $ Int 1)
        whileCond = LessEq (Var name) (Var "_to")
        whileBody = (postScript body ++ [incrI]) 

postStmt stmt = [stmt]