-- Sprockell "documentation":
-- https://github.com/bobismijnnaam/sprockell/blob/master/src/Sprockell/HardwareTypes.hs#L115

{- special registers: -}

-- reg0         always zero
-- regSprID     contains the sprockellID
-- regSP        stack pointer
-- regPC        program counter

data AddrImmDI  
    = ImmValue Int          -- immediate value
    | DirAddr MemAddr       -- memory address
    | IndAddr RegAddr       -- register address

data Target 
    = Abs CodeAddr          -- absolute position
    | Rel CodeAddr          -- relative position
    | Ind RegAddr           -- position from register

data Operator    
    = Add   | Sub | Mul
    | Equal | NEq | Gt  | Lt     | GtE | LtE
    | And   | Or  | Xor | LShift | RShift
    | Decr  | Incr

data Instruction 
    = Compute       Operator RegAddr RegAddr RegAddr
    | Jump          Target
    | Branch        RegAddr Target
    | Load          AddrImmDI RegAddr
    | Store         RegAddr AddrImmDI
    | Push          RegAddr                 -- push to stack
    | Pop           RegAddr                 -- pop from stack
    | ReadInstr     AddrImmDI               -- for reading from shared memory (e.g. numberIO, charIO)
    | Receive       RegAddr                 -- wait for reply and save to register 
    | WriteInstr    RegAddr AddrImmDI       -- for writing to shared memory
    | TestAndSet    AddrImmDI               -- used for synchronization/locking
    | EndProg
    | Nop
