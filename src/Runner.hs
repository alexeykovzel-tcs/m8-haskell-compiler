module Runner where

import Sprockell
import Compiler
import Control.Monad (join)

runFile :: FilePath -> IO() 
runFile file = runFiles 1 file

runFiles :: Int -> FilePath -> IO()
runFiles n file = join $ runProgs n <$> readFile file

runProgs :: Int -> String -> IO()
runProgs n code = run $ replicate n $ compile code 

debugFile :: FilePath -> IO()
debugFile file = join $ debugProg <$> readFile file

debugProg :: String -> IO()
debugProg code = runWithDebugger 
    (debuggerSimplePrint showLocalMem) 
    [compile code]

showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) = 
    show $ localMem $ head $ sprStates systemState
    
printFileASM :: FilePath -> IO()
printFileASM file = join $ printASM <$> compile <$> readFile file

printASM :: [Instruction] -> IO()
printASM asm = do
    putStrLn ""
    mapM_ putStrLn $ zipWith (\i instr -> show i ++ ": " ++ show instr) [0..] asm
    putStrLn ""