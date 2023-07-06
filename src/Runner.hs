module Runner where

-- author: Aliaksei Kouzel - s2648563

import Sprockell
import Compiler
import Parser (parseWith, script)

-----------------------------------------------------------------------------
-- ASM printer
-----------------------------------------------------------------------------

-- prints the compiled program from a file
printFileASM :: FilePath -> IO()
printFileASM file = do 
    code <- readFile file
    printASM $ compile 1 code

-- prints the compiled program
printASM :: [Instruction] -> IO()
printASM asm = do
    putStrLn ""
    mapM_ putStrLn $ zipWith printInstr [0..] asm
    putStrLn ""

-- prints an ASM instruction
printInstr :: Int -> Instruction -> String
printInstr i instr = show i ++ ": " ++ show instr

-----------------------------------------------------------------------------
-- program runner
-----------------------------------------------------------------------------

-- runs a program from a file
runFile :: FilePath -> IO()
runFile file = do
    code <- readFile file
    runProg code

-- runs a program from a string
runProg :: String -> IO()
runProg = run . prepProg

-- parsers and compiles a program for multiple processes
prepProg :: String -> [[Instruction]]
prepProg code = replicate num $ prog
    where
        ast   = parseWith script code
        num   = fromInteger $ countWorkers ast + 1
        prog  = compileAST num ast

-----------------------------------------------------------------------------
-- program debugger
-----------------------------------------------------------------------------

-- runs a program from a file with terminal output
debugFile :: FilePath -> IO()
debugFile file = do
    code <- readFile file
    debugProg code

-- runs a program from a string with terminal output
debugProg :: String -> IO()
debugProg code = runWithDebugger 
    (debuggerSimplePrint showSharedMem) 
    (prepProg code)

-- debugger that pritns the local memory of the main process
showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) = 
    show $ localMem $ head $ sprStates systemState
    
-- debugger that prints the shared memory
showSharedMem :: DbgInput -> String
showSharedMem (_, systemState) = 
    show $ sharedMem systemState