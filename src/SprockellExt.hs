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

type ScopeID    = Integer
type ScopePath  = Map ScopeID ScopeID
type ScopeMap   = Map ScopeID (VarMap, Depth, Size)

type FunMap     = Map FunName (ScopeID, CodeAddr)

data Context = Ctx {
    ctxScopeId :: ScopeID,
    funMap     :: FunMap,
    scopeMap   :: ScopeMap,
    scopePath  :: ScopePath,
    freeRegs   :: [RegAddr]
}

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
occupyReg (Ctx a b c d (r:rs)) = (r, Ctx a b c d rs)

-- finds a free register
findReg :: Context -> RegAddr
findReg ctx = let (r:_) = freeRegs ctx in r

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

-----------------------------------------------------------------------------
-- memory management
-----------------------------------------------------------------------------

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

-- loads a variable from memory to a register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = loadVarAtIdx ctx name reg 0

-- loads a variable from memory to a register at the given index
loadVarAtIdx :: Context -> VarName -> RegAddr -> Integer -> [Instruction]
loadVarAtIdx ctx name reg idx = applyVar ctx name 
    $ \ctx regArp offset -> loadAI ctx regArp (offset + idx) reg

-- updates a variable from a register
putVar :: Context -> VarName -> RegAddr -> [Instruction]
putVar ctx name reg = putVarAtIdx ctx name reg 0

-- updates a variable from a register at the given index
putVarAtIdx :: Context -> VarName -> RegAddr -> Integer -> [Instruction]
putVarAtIdx ctx name reg idx = applyVar ctx name
    $ \ctx regArp offset -> storeAI ctx reg regArp (offset + idx)

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
getVar ctx name = fromJust $ Map.lookup name varMap
    where (varMap, _, _) = getScope ctx

-- gets current scope information
getScope :: Context -> (VarMap, Depth, Size)
getScope ctx = fromJust $ Map.lookup (ctxScopeId ctx) (scopeMap ctx)

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

-- updates register with a data pointer
setNextArp :: Context -> [Instruction]
setNextArp ctx = [loadImm scopeSize reg, Compute Add reg regArp regArp]
    where  
        (_, _, scopeSize) = getScope ctx
        reg = findReg ctx

-- updates the current scope
applyScope :: (ScopeID -> ScopeID) -> Context -> Context
applyScope apply (Ctx scope a b c d) = Ctx (apply scope) a b c d

-- updates context for the next scope
inScopeCtx :: Context -> Context
inScopeCtx = applyScope (\id -> id + 1)

-- updates context for the next "else" scope
inScopeCtxElse :: Context -> Context
inScopeCtxElse ctx = applyScope (\_ -> nextId) ctx
    where 
        id     = ctxScopeId ctx
        nextId = fromJust $ Map.lookup id (scopePath ctx)