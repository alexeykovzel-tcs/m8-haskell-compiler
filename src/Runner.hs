module Runner where

import Sprockell
import Compiler
import Control.Monad (join)

runFile :: FilePath -> IO()
runFile file = join $ runProg <$> readFile file

debugFile :: FilePath -> IO()
debugFile file = join $ debugProg <$> readFile file

printFileASM :: FilePath -> IO()
printFileASM file = join $ printASM <$> readFile file

runProg :: String -> IO()
runProg code = run [compile code]

debugProg :: String -> IO()
debugProg code = runWithDebugger 
    (debuggerSimplePrint showLocalMem) 
    [compile code]

printASM :: String -> IO()
printASM code = do
    putStrLn ""
    mapM_ print (compile code)
    putStrLn ""

showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) = 
    show $ localMem $ head $ sprStates systemState