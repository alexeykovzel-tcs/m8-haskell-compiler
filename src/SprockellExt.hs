module SprockellExt where

import Sprockell
import Debug.Trace
import Data.Maybe
import Data.Char (ord)
import Data.List (sort)
import Data.Map (Map)
import Parser (VarName, FunName)
import qualified Data.Map as Map 

type Size       = Integer
type Offset     = Integer
type Depth      = Integer

type VarSize    = Integer
type VarPos     = (Depth, Offset)
type VarMap     = Map VarName (VarPos, VarSize)
type GlVarMap   = Map VarName (MemAddr, VarSize)

type ScopeID    = Integer
type ScopePath  = Map ScopeID ScopeID
type ScopeMap   = Map ScopeID (VarMap, Depth, Size)

type FunMap     = Map FunName (ScopeID, Depth, [VarName])

data Context = Ctx {
    scopeId      :: ScopeID,
    peerId       :: ScopeID,
    funMap       :: FunMap,
    glVars       :: GlVarMap, 
    scopeMap     :: ScopeMap,
    scopePath    :: ScopePath,
    freeRegs     :: [RegAddr],
    freeWorkers  :: [MemAddr]
}

startProg :: [Instruction]
startProg = initArp ++ initWorkers

endProg :: Context -> [Instruction]
endProg ctx = endProcs ctx ++ [EndProg]

-----------------------------------------------------------------------------
-- process management
-----------------------------------------------------------------------------

startWorkers :: Context -> [MemAddr] -> [Instruction]
startWorkers ctx workers = let reg = findReg ctx in
    concat $ reverse $ (\idx -> 
        let addr = workers !! (idx - 1) in 
        lockMem reg addr ++ [
            Load (ImmValue $ 7 * idx) reg,
            Compute Add regPC reg reg,
            WriteInstr reg (DirAddr addr)
        ]) <$> [1..length workers]

joinWorkers :: Context -> [MemAddr] -> [Instruction] 
joinWorkers ctx workers = let reg = findReg ctx in 
    concat $ (\addr -> 
        readGlMem addr reg ++ [
        Compute NEq reg0 reg reg,
        Branch reg (Rel $ -3)
    ]) <$> workers

occupyWorkers :: Context -> Int -> ([MemAddr], Context)
occupyWorkers ctx num = 
    let workers = freeWorkers ctx
    in (take num workers, ctx { freeWorkers = drop num workers })

initWorkers :: [Instruction]
initWorkers = [
        -- skip busy wait if the main process
        Compute Equal reg0 regSprID regB,
        Branch regB (Rel $ length busyWait + 1)
    ]
    ++ busyWait
    where 
        busyWait = [
            -- reset assigned address
            WriteInstr reg0 (IndAddr regSprID),

            -- busy wait for sub processes
            ReadInstr (IndAddr regSprID),
            Receive regB,

            -- handle termination signal
            Compute GtE regB reg0 regC,
            Branch regC (Rel 2),
            EndProg,

            -- check if not assigned
            Compute Equal regB reg0 regC,
            Branch regC (Rel 5),

            -- check if reserved
            Load (ImmValue 1) regC,
            Compute Equal regB regC regC,
            Branch regC (Rel 2),

            -- jump to the assigned address
            Jump (Ind regB),

            -- jump to busy wait
            Jump $ Abs 4 ]

endProcs :: Context -> [Instruction]
endProcs ctx = let reg = findReg ctx in 
      Load (ImmValue $ -1) reg
    : (WriteInstr reg <$> DirAddr <$> freeWorkers ctx)

-----------------------------------------------------------------------------
-- register management
-----------------------------------------------------------------------------

-- reserves ARP register
regArp = regA

-- reserves registers for the free usage
userRegs = [regB, regC, regD, regE, regF]

initArp :: [Instruction]
initArp = [Load (ImmValue 0) regArp]

-- occupies a free register
occupyReg :: Context -> (RegAddr, Context)
occupyReg ctx = (r, ctx {freeRegs = rs})
    where (r:rs) = freeRegs ctx

-- finds a free register
findReg :: Context -> RegAddr
findReg ctx = case freeRegs ctx of
    []      -> error "no registers left :/"
    (r:_)   -> r

-- copies value from register 1 to register 2
copyReg :: RegAddr -> RegAddr -> Instruction
copyReg reg1 reg2 = Compute Add reg0 reg1 reg2

-- loads value to a register
loadImm :: Integer -> RegAddr -> Instruction
loadImm val reg = Load (ImmValue $ fromInteger val) reg

-- adds value to a register
addImm :: Context -> Integer -> RegAddr -> [Instruction]
addImm ctx val reg = let reg2 = findReg ctx in 
    [loadImm val reg2, Compute Add reg reg2 reg]

-- branches over the given instructions
branchOver :: RegAddr -> [Instruction] -> [Instruction]
branchOver reg body = [Branch reg $ Rel $ length body + 1]

-- jumps over the given instructions
jumpOver :: [Instruction] -> [Instruction]
jumpOver body = [Jump $ Rel $ length body + 1]

-- jumps back over the given sets of intructions
jumpBack :: [[Instruction]] -> [Instruction]
jumpBack parts = [Jump $ Rel $ -partsLen + 1]
    where partsLen = foldl (\a b -> a + length b) 0 parts

-- jumps to the address stored in the variable
jumpVar :: Context -> VarName -> [Instruction]
jumpVar ctx name = loadVar ctx2 name reg2 ++ [Jump $ Ind reg2]
    where (reg2, ctx2) = occupyReg ctx

negateReg :: RegAddr -> [Instruction]
negateReg reg = [Compute Sprockell.Sub reg0 reg reg]

-----------------------------------------------------------------------------
-- memory management
-----------------------------------------------------------------------------

readGlMem :: MemAddr -> RegAddr -> [Instruction]
readGlMem addr reg = [ReadInstr (DirAddr addr), Receive reg]

lockMem :: RegAddr -> MemAddr -> [Instruction]
lockMem reg addr = [
        TestAndSet (DirAddr addr), 
        Receive reg, 
        Compute Equal reg reg0 reg,
        Branch reg (Rel $ -3) 
    ]

unlockMem :: MemAddr -> [Instruction]
unlockMem addr = [WriteInstr reg0 (DirAddr addr)]

-- increments a variable in memory
incrMem :: Context -> VarName -> [Instruction]
incrMem ctx name = addMem ctx name 1

-- adds value to a variable in memory
addMem :: Context -> VarName -> Integer -> [Instruction]
addMem ctx name val = let (reg2, ctx2) = occupyReg ctx in
       loadVar ctx2 name reg2
    ++ addImm  ctx2 val  reg2 
    ++ putVar  ctx2 name reg2

-- loads from memory with an offset
loadAI :: Context -> RegAddr -> Offset -> RegAddr -> [Instruction]
loadAI ctx reg1 offset reg2 = let reg3 = findReg ctx
    in [
        loadImm offset reg3,
        Compute Add reg1 reg3 reg3,
        Load (IndAddr reg3) reg2
    ]

-- stores to memory with an offset
storeAI :: Context -> RegAddr -> RegAddr -> Offset -> [Instruction]
storeAI ctx reg1 reg2 offset = let reg3 = findReg ctx
    in [
        loadImm offset reg3,
        Compute Add reg3 reg2 reg3,
        Store reg1 (IndAddr reg3)
    ]

-----------------------------------------------------------------------------
-- IO instructions
-----------------------------------------------------------------------------

printVarStr :: Context -> VarName -> [Instruction] 
printVarStr ctx name = let reg = findReg ctx in
    applyArr ctx name (\reg -> [WriteInstr reg charIO])
    ++ printChar reg '\n'

-- prints a value from a register
printReg :: RegAddr -> [Instruction]
printReg reg = [WriteInstr reg numberIO]

-- prints a string
printStr :: Context -> String -> [Instruction]
printStr ctx str = concat $ map (printChar $ findReg ctx) str

-- prints a string with a new line
printStrLn :: Context -> String -> [Instruction]
printStrLn ctx str = printStr ctx (str ++ "\n")

-- prints a single character
printChar :: RegAddr -> Char -> [Instruction]
printChar reg c = [Load (ImmValue $ ord c) reg, WriteInstr reg charIO]

-----------------------------------------------------------------------------
-- variable management
-----------------------------------------------------------------------------

glVarAddr :: Context -> VarName -> MemAddr
glVarAddr ctx name = addr
    where (addr, _) = fromJust $ Map.lookup name (glVars ctx)

-- loads a variable from memory to a register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = loadVarAtIdx ctx name reg 0

-- loads a variable from memory to a register at the given index
loadVarAtIdx :: Context -> VarName -> RegAddr -> Integer -> [Instruction]
loadVarAtIdx ctx name reg idx = case Map.lookup name (glVars ctx) of
    Just (addr, _) -> readGlMem (addr + fromInteger idx) reg
    Nothing -> applyVar ctx name 
        $ \ctx regArp offset -> 
            loadAI ctx regArp (offset + idx) reg

-- update a variable from a register + value
putVarAdd :: Context -> VarName -> RegAddr -> Integer -> [Instruction]
putVarAdd ctx name reg val = let (reg2, ctx2) = occupyReg ctx
    in copyReg reg reg2
    :  addImm ctx2 val reg2
    ++ putVar ctx2 name reg2

-- updates a variable from a register
putVar :: Context -> VarName -> RegAddr -> [Instruction]
putVar ctx name reg = putVarAtIdx ctx name reg 0

-- updates a variable from a register at the given index
putVarAtIdx :: Context -> VarName -> RegAddr -> Integer -> [Instruction]
putVarAtIdx ctx name reg idx = applyVar ctx name
    $ \ctx regArp offset -> storeAI ctx reg regArp (offset + idx)

-- stores a program counter in a variable
putPC :: Context -> VarName -> [Instruction]
putPC ctx name = applyVar ctx name
    $ \ctx arpReg offset -> 
    let
        (reg2, ctx2) = occupyReg ctx 
        reg3 = findReg ctx2
    in [
        loadImm offset reg2,
        Compute Add reg2 arpReg arpReg,
        copyReg regPC reg2,
        loadImm 5 reg3,
        Compute Add reg2 reg3 reg2,
        Store reg2 (IndAddr arpReg)
    ]

-- update an array with values
putArrImm :: Context -> VarName -> [Integer] -> [Instruction]
putArrImm ctx name vals = applyVar ctx name
    $ \ctx regArp offset -> let 

        (reg2, ctx2) = occupyReg ctx
        valToMem idx = loadImm (vals !! idx) reg2
                     : storeAI ctx2 reg2 regArp (offset + toInteger idx)
        
        in foldl1 (++) $ map valToMem [0..length vals - 1]

-- applies a function to each element of the array
applyArr :: Context -> VarName -> (RegAddr -> [Instruction]) -> [Instruction]
applyArr ctx name applyIdx = applyVar ctx name
    $ \ctx regArp offset -> foldl1 (++) $ apply <$> [offset..offset + size - 1]
    where 
        (_, size)     = getVar ctx name
        (reg2, ctx2)  = occupyReg ctx
        apply offset  = loadAI ctx2 regArp offset reg2 ++ applyIdx reg2

-- loads a variable ARP and applies a function to it
applyVar :: Context -> VarName -> (Context -> RegAddr -> Offset -> [Instruction]) -> [Instruction]
applyVar ctx name apply = arpToReg ++ apply ctx2 reg2 offset
    where
        ((depth, offset), _) = getVar ctx name
        (reg2, ctx2)         = occupyReg ctx
        (_, scopeDepth, _)   = getScope ctx
        arpToReg             = loadArp ctx2 (scopeDepth - depth) reg2

-- loads a data pointer at the given scope depth
loadArp :: Context -> Depth -> RegAddr -> [Instruction]
loadArp ctx 0 reg = [copyReg regArp reg]
loadArp ctx 1 reg = loadAI ctx regArp (-1) reg
loadArp ctx n reg = loadArp ctx (n - 1) reg ++ loadAI ctx reg (-1) reg

-- gets variable information by name
getVar :: Context -> VarName -> (VarPos, VarSize)
getVar ctx name = 
    let (varMap, _, _) = getScope ctx
    in case Map.lookup name varMap of
        Just var  -> var
        Nothing   -> error $ "no such var: " ++ name 
                    ++ " " ++ (show $ scopeId ctx)

-- gets current scope information
getScope :: Context -> (VarMap, Depth, Size)
getScope ctx = fromJust $ Map.lookup (scopeId ctx) (scopeMap ctx)

-----------------------------------------------------------------------------
-- scope management
-----------------------------------------------------------------------------

-- puts instructions inside a scope
putInScope :: Context -> [Instruction] -> [Instruction]
putInScope ctx body = 
       putVar ctx "_arp" regArp
    ++ setNextArp ctx 
    ++ body 
    ++ loadAI ctx regArp (-1) regArp

-- updates register with an ARP pointer
setNextArp :: Context -> [Instruction]
setNextArp ctx = [loadImm scopeSize reg, Compute Add reg regArp regArp]
    where  
        (_, _, scopeSize) = getScope ctx
        reg = findReg ctx

childCtx :: Context -> Context
childCtx ctx = ctx { scopeId = peerId ctx, peerId = peerId ctx + 1 }

peerCtx :: Context -> Context
peerCtx ctx = nextPeer $ ctx { scopeId = updatePeer ctx $ scopeId ctx + 1 }

nextPeer :: Context -> Context
nextPeer ctx = ctx { peerId = updatePeer ctx $ peerId ctx }

updatePeer :: Context -> ScopeID -> ScopeID
updatePeer ctx id = case Map.lookup id (scopePath ctx) of
    Just peerId -> peerId
    Nothing -> error $ "no such path: " ++ show id