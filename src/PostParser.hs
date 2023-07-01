{-# LANGUAGE FlexibleInstances #-}

module PostParser where

import Parser

class PostParsable a where
    postParse :: a -> a

instance PostParsable Script where
    postParse [] = []
    postParse (x:xs) = postParse x : postParse xs

instance PostParsable Statement where
    
    postParse (WhileLoop cond body) = WhileLoop cond (postParse body)
    
    postParse (FunDef name args returnType body) = 
        FunDef name args returnType (postParse body)
    
    postParse (Condition cond ifBody Nothing) = 
        Condition cond (postParse ifBody) Nothing
    
    postParse (Condition cond ifBody (Just elseBody)) = 
        Condition cond (postParse ifBody) (Just $ postParse elseBody)
    
    postParse (InScope body) = InScope (postParse body)

    -- change "for" loop to "while" as it's easier to compile
    postParse (ForLoop i@(name, _) (IterRange from to) body) = 
        InScope [
            VarDecl i (Just $ from),
            VarDecl ("_to", IntType) (Just $ to),
            WhileLoop whileCond whileBody ]
        where 
            incrI     = VarAssign name $ Add (Var name) (Fixed $ Int 1)
            whileCond = LessEq (Var name) (Var "_to")
            whileBody = (postParse body ++ [incrI]) 

    postParse stmt = stmt