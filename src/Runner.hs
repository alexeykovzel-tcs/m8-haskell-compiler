module Runner where

import Sprockell
import Compiler
import Parser (parseWith, script)

runFile :: FilePath -> IO()
runFile file = do
    code <- readFile file
    runProg code

runProg :: String -> IO()
runProg = run . buildProg

debugFile :: FilePath -> IO()
debugFile file = do
    code <- readFile file
    debugProg code

debugProg :: String -> IO()
debugProg code = runWithDebugger 
    (debuggerSimplePrint showSharedMem) 
    (buildProg code)

buildProg :: String -> [[Instruction]]
buildProg code = replicate num $ prog
    where
        ast   = parseWith script code
        num   = fromInteger $ countWorkers ast + 1
        prog  = compileAST num ast

showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) = 
    show $ localMem $ head $ sprStates systemState
    
showSharedMem :: DbgInput -> String
showSharedMem (_, systemState) = 
    show $ sharedMem systemState

printFileASM :: FilePath -> IO()
printFileASM file = do 
    code <- readFile file
    printASM $ compile 1 code

printASM :: [Instruction] -> IO()
printASM asm = do
    putStrLn ""
    mapM_ putStrLn $ zipWith printInstr [0..] asm
    putStrLn ""

printInstr :: Int -> Instruction -> String
printInstr i instr = show i ++ ": " ++ show instr