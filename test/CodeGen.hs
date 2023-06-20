{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module CodeGen where

import GHC.Generics
import FPPrac.Trees

-- It is just an example...

-----------------------------------------------------------------------------
-- code generation
-----------------------------------------------------------------------------

type Script = [Statement]

data Statement
    = Assign    Int Expr
    | Exec      Expr
    | Repeat    Expr Script
    deriving (Show, Generic, ToRoseTree)

data Expr 
    = Const     Int
    | Var       Int
    | BinExpr   Op Expr Expr
    deriving (Show, Generic, ToRoseTree)

class CodeGen a where
    codegen :: a -> [Instr]

instance CodeGen Script where
    codegen [] = []
    codegen (x:xs) = codegen x ++ codegen xs

instance CodeGen Statement where
    codegen (Assign var expr) = codegen expr ++ [Store var]
    codegen (Exec expr) = codegen expr
    codegen (Repeat i script) 
        = PushPC : codegen i 
        ++ codegen script ++ [EndRep]

instance CodeGen Expr where
    codegen (Const x) = [PushConst x]
    codegen (Var x) = [PushAddr x]
    codegen (BinExpr op x y) 
        = codegen x ++ codegen y ++ [Calc op]

compile :: Script -> [Instr]
compile [] = [EndProg]
compile (x:xs) = codegen x ++ compile xs

-----------------------------------------------------------------------------
-- testing examples
-----------------------------------------------------------------------------

testSum :: Int -> Script
testSum x = [
    Assign 0 (Const 0), 
    Repeat (Const $ x - 1) [
        Assign 0 (BinExpr Add (Var 0) (Const 1)) 
    ]]

debug :: Script -> IO()
debug script 
    = putStr 
    $ unlines
    $ map show
    $ takeWhile (\(pc, _, _, _) -> pc /= -1)
    $ scanl instr memory clock 
        where 
            memory  = (0, 0, heap, stack)
            heap    = replicate 8 0
            stack   = replicate 8 0
            instr   = core $ compile script
            clock   = repeat Tick

-----------------------------------------------------------------------------
-- simple processor
-----------------------------------------------------------------------------

data Tick = Tick

-- Program counter, Stack pointer, Heap, Stack 
type Memory = (Int, Int, [Int], [Int])

data Op = Add | Mul | Sub
    deriving (Show, Generic, ToRoseTree)

data Instr  
    = PushConst Int
    | PushAddr Int
    | Store Int
    | Calc Op
    | PushPC
    | EndRep
    | EndProg
    deriving (Show, Generic, ToRoseTree)

-- Put value a on position i in list xs
xs <~ (i, a) = take i xs ++ [a] ++ drop (i + 1) xs

alu op = case op of
    Add -> (+)
    Mul -> (*)
    Sub -> (-)

core :: [Instr] -> Memory -> Tick -> Memory
core instr (pc, sp, heap, stack) tick = case instr!!pc of

    -- Push constant value onto the stack
    PushConst x -> (pc + 1, sp + 1, heap, stack <~ (sp, x))

    -- Fetch value from the heap under the given 
    -- address and push it onto the stack
    PushAddr addr -> (pc + 1, sp + 1, heap, stack <~ (sp, heap!!addr)) 

    -- Pop value from the stack and store it 
    -- in the heap under the given address
    Store addr -> (pc + 1, sp - 1, heap <~ (addr, x), stack)
            where 
                x = stack!!(sp - 1)

    -- Perform calculation with the last two stack values, 
    -- then push the result back onto the stack
    Calc op -> (pc + 1, sp - 1, heap, stack <~ (sp - 2, result))
            where 
                result = alu op x y
                x = stack!!(sp - 2)
                y = stack!!(sp - 1)
    
    -- Push the current program counter onto the stack
    PushPC -> (pc + 1, sp + 1, heap, stack <~ (sp, pc + 1))

    -- Put program counter to the beginning of the cycle,
    -- if there are any iterations left, otherwise continue
    EndRep  | i == 0    -> (pc + 1, sp - 2, heap, stack)
            | otherwise -> (pcBack + 1, sp, heap, stack <~ (sp - 1, i - 1))
            where 
                pcBack  = stack!!(sp - 2)
                i       = stack!!(sp - 1)

    -- Terminate the program
    EndProg -> (-1, sp, heap, stack)