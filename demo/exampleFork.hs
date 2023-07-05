module Main where

import Sprockell

-- start the program with begin
-- if not main thread, jump to busy wait
-- busy wait ends once receive which part of the code to jump to
-- at the end of the forked code, jump to busy wait again
-- if main is going to end, jump to endprog
prog :: [Instruction]
prog = [
        Branch regSprID (Rel 10), -- 0: if not main, jump to busy wait
        
        Load (ImmValue 5) regA,  -- 1: x = 5
        WriteInstr regA (DirAddr 3), -- 2: write x to memory

        Load (ImmValue 15) regB, -- 3: jump of sp1 -------------------------------
        WriteInstr regB (DirAddr 1), -- 4: jump to f1
        Store regB (DirAddr 1), -- 5: store f1 in memory

        Load (ImmValue 23) regB, -- 6: jump of sp2 -------------------------------
        WriteInstr regB (DirAddr 2), -- 7: jump to f2
        Store regB (DirAddr 2), -- 8: store f2 in memory

        Jump (Abs 31), -- 9: jump to endprog ----------------------------------------

        ReadInstr (IndAddr regSprID), -- 10: busy wait
        Receive regA,   -- 11: receive which part of the code to jump to
        Compute Equal regA regB regC, -- 12:
        Branch regC (Rel (-3)), -- 13: if not equal then jump to busy wait
        Jump (Ind regA), -- 14: jump

        ReadInstr (DirAddr 3), -- 15: f1: read x
        Receive regC, -- 16:
        Load (ImmValue 2) regB, -- 17: load constant
        Compute Add regC regB regD, -- 18: add x + 2 
        WriteInstr regD (DirAddr 3), -- 19: write x to memory
        Load (ImmValue 1) regB, -- 20: load constant
        WriteInstr regB (DirAddr 1), -- 21: notify that f1 is done
        Jump (Abs 10), -- 22: jump to busy wait ----------------------------------------

        ReadInstr (DirAddr 3), -- 23: f2: read x
        Receive regC, -- 24:
        Load (ImmValue 3) regB, -- 25: load constant
        Compute Add regC regB regD, -- 26: add x + 3
        WriteInstr regD (DirAddr 3), -- 27: write x to memory
        Load (ImmValue 2) regB, -- 28: load constant
        WriteInstr regB (DirAddr 2), -- 29: notify that f2 is done
        Jump (Abs 10), -- 30: jump to busy wait ----------------------------------------

        ReadInstr (DirAddr 1), -- 31: busy wait for f1
        Receive regB, -- 32: here's the continuation of the corrected code:
        Load (DirAddr 1) regC, -- 33: load f1 from memory
        Compute Equal regB regC regD, -- 34:
        Branch regD (Rel (-4)), -- 35: if not equal then jump to busy wait

        ReadInstr (DirAddr 2), -- 36: busy wait for f2
        Receive regB, -- 37: receive if f2 is done
        Load (DirAddr 2) regB, -- 38: load f2 from memory
        Compute Equal regB regC regD, -- 39:
        Branch regD (Rel (-4)), -- 40: if not equal then jump to busy wait

        Load (ImmValue 47) regD, -- 41: jump to endprog ----------------------------------------
        WriteInstr regD (DirAddr 1), -- 42: jump to endprog
        WriteInstr regD (DirAddr 2), -- 43: jump to endprog
        ReadInstr (DirAddr 3), -- 44: read x
        Receive regA, -- 45: receive x
        WriteInstr regA numberIO, -- 46: write x to output
        EndProg -- 47: endprog
        ]

main = runWithDebugger (debuggerSimplePrintAndWait myShow) [prog, prog, prog]
