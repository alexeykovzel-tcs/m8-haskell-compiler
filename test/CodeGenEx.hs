module Ex.CodeGen where

import Sprockell
import Data.Char

prog_fib :: [Instruction]
prog_fib = [ 
    ReadInstr numberIO,             -- ask the user for a number
    Receive regE,                   -- save the number in regE

    Load (ImmValue 0) regA,         -- first number
    Load (ImmValue 1) regB,         -- second number

    -- "beginloop"
    Compute Gt regA regE regC,      -- regA > regE ?
    Branch regC (Abs 13),           -- then jump to target "end"
    WriteInstr regA numberIO,       -- output regA
    Compute Add regA regB regA,
    Compute Gt regB regE regC,      -- regB > regE
    Branch regC (Abs 13),           -- target "end"
    WriteInstr regB numberIO,       -- output regB
    Compute Add regA regB regB,
    Jump (Rel (-8)),                -- target "beginloop"

    -- "end"
    EndProg ]

run_fib = run [prog_fib]

prog_mult :: [Instruction]
prog_mult = [
    Branch regSprID (Rel 6),        -- target "beginLoop"
    Load (ImmValue 13) regC,
    WriteInstr regC (DirAddr 1),    -- Sprockell 1 must jump to second EndProg
    WriteInstr regC (DirAddr 2),    -- Sprockell 2 must jump to second EndProg
    WriteInstr regC (DirAddr 3),    -- Sprockell 3 must jump to second EndProg
    Jump (Abs 12),                  -- Sprockell 0 jumps to first EndProg
    
    -- beginLoop
    ReadInstr (IndAddr regSprID),
    Receive regA,
    Compute Equal regA reg0 regB,
    Branch regB (Rel (-3)),
    
    -- endLoop
    WriteInstr regA numberIO,
    Jump (Ind regA),

    -- 12: Sprockell 0 is sent here
    EndProg,

    -- 13: Sprockells 1, 2 and 3 are sent here
    EndProg ]

run_mult = runWithDebugger 
    (debuggerSimplePrint myShow) 
    [prog_mult, prog_mult, prog_mult, prog_mult]

prog_charIO :: [Instruction]
prog_charIO = 
    writeString "What is your name? " ++ [

    -- ASCII code newline in regE for later reference
    Load (ImmValue $ ord '\n') regE,

    -- "beginInputLoop": 39
    ReadInstr charIO,               -- Request a character from stdin
    Receive regA,                   -- Save it in regA (as ASCII code)
    Branch regA (Rel 2),
    Jump (Rel (-3)),                -- got 0, no input available, try again

    -- got input char
    Compute Equal regA regE regC,   -- check if it's a newline (remember: in regE)
    Branch regC (Rel 4),            -- then jump to "inputDone"
    Store regA (IndAddr regB),      -- else store character in local memory
    Compute Incr regB regB regB,
    Jump (Rel (-8)) ]               -- "beginInputLoop"

    -- "inputDone"
    ++ writeString "Hello " ++ [

    -- "beginLoopOutput"
    Load (IndAddr regD) regA,
    WriteInstr regA charIO,
    Compute Incr regD regD regD,
    Compute NEq regB regD regC,
    Branch regC (Rel (-4)) ]        -- target "loopOutput"

    ++ writeString "!\n"
    ++ [EndProg]

-- | Generate code to print a (Haskell) String
writeString :: String -> [Instruction]
writeString str = concat $ map writeChar str

-- | Generate code to print a single character
writeChar :: Char -> [Instruction]
writeChar c = [ 
    Load (ImmValue $ ord c) regA, 
    WriteInstr regA charIO ]

showLocalMem :: DbgInput -> String
showLocalMem ( _ , systemState ) = show $ localMem $ head $ sprStates systemState

run_charIO = run [prog_charIO]
debug_charIO = runWithDebugger noDebugger [prog_charIO]
debug_charIO_print = runWithDebugger 
    (debuggerSimplePrint showLocalMem) [prog_charIO]