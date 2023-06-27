module Test.CodeGen where

import Sprockell
import Elaborator
import CodeGen
import qualified Parser as AST
import qualified Data.Map as Map

runExpr :: String -> IO()
runExpr str = run [strToExpr str]

runProg :: String -> IO()
runProg str = run [strToProg str]

debugProg :: String -> IO()
debugProg str = runWithDebugger 
    (debuggerSimplePrint showLocalMem) 
    [strToProg str]

showProg :: String -> IO()
showProg str = do
    putStrLn ""
    mapM_ print (strToProg str)
    putStrLn ""

-----------------------------------------------------------------------------

strToProg :: String -> [Instruction]
strToProg prog = compile (testCtx vars) ast
    where 
        ast = AST.tryParse AST.script prog
        vars = testCoords 0 ast

strToExpr :: String -> [Instruction]
strToExpr expr = code ++ printResult
    where
        ast = AST.tryParse AST.expr expr
        code = compileExpr (testCtx []) ast regB
        printResult = [WriteInstr regB numberIO, EndProg]

testCtx :: [(AST.VarName, VarCoord)] -> Context
testCtx vars = Ctx (0, 1) funMap varMap regs
    where
        regs = [regC, regD, regE, regF]
        varMap = Map.fromList [(0, Map.fromList vars)]
        funMap = Map.empty

testCoords :: Int -> AST.Script -> [(AST.VarName, VarCoord)]
testCoords offset ((AST.VarDecl (name, _) expr):xs) = entry : rest
    where 
        entry = (name, (1, offset))
        rest = testCoords (offset + 1) xs

testCoords offset (_:xs) = testCoords offset xs
testCoords _ [] = []

showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) = 
    show $ localMem $ head $ sprStates systemState