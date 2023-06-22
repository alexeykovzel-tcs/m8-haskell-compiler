module CodeGen ( compile ) where

import Sprockell

-- Sprockell instructions, memory types, etc.:
-- https://github.com/bobismijnnaam/sprockell/blob/master/src/Sprockell/HardwareTypes.hs#L115

compile :: Integer -> [Instruction]
compile n = [ 
        Load (ImmValue $ fromInteger n) regE,
        Load (ImmValue 0) regA,
        Load (ImmValue 1) regB,
        Compute Gt regA regE regC,
        Branch regC (Abs 12),
        WriteInstr regA numberIO,
        Compute Add regA regB regA,
        Compute Gt regB regE regC,
        Branch regC (Abs 12),
        WriteInstr regB numberIO,
        Compute Add regA regB regB,
        Jump (Rel (-8)),
        EndProg
    ]
