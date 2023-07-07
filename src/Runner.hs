module Runner where

{- author: Aliaksei Kouzel - s2648563 -}

import Sprockell
import Compiler

-----------------------------------------------------------------------------
-- ASM printer
-----------------------------------------------------------------------------

-- prints the compiled program from a file
printFileASM :: FilePath -> IO()
printFileASM file = do
    prog <- compileFile file 
    printASM $ prog !! 0

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
    prog <- compileFile file
    run prog

-- runs a program from a string
runProg :: String -> IO()
runProg = run . compile

-- runs a function from a file
runFileFun :: FilePath -> String -> [Integer] -> IO() 
runFileFun file name args = do
    code <- readFile file
    run [compileFunCall code name args]

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
    (compile code)

-- debugger that pritns the local memory of the main process
showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) = 
    show $ localMem $ head $ sprStates systemState
    
-- debugger that prints the shared memory
showSharedMem :: DbgInput -> String
showSharedMem (_, systemState) = 
    show $ sharedMem systemState