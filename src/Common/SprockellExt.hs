module Common.SprockellExt where

import Sprockell
import Debug.Trace
import Data.Maybe
import Data.Char (ord)
import Data.List (sort)
import Data.Map (Map)
import Parser (VarName)
import qualified Data.Map as Map 

type Offset     = Integer
type Depth      = Integer

type VarSize    = Integer
type VarPos     = (Depth, Offset)
type VarMap     = Map VarName (VarPos, VarSize)

type Scope      = (ScopeID, Depth)
type ScopeID    = Integer
type ScopeSize  = Integer
type ScopePath  = [(ScopeID, ScopeID)]
type ScopeInfo  = (VarMap, ScopeSize, ScopeID)
type ScopeMap   = Map ScopeID ScopeInfo

data Context = Ctx {
    ctxScope  :: Scope,
    scopeMap  :: ScopeMap,
    scopePath :: ScopePath,
    freeRegs  :: [RegAddr]
}

-----------------------------------------------------------------------------
-- register management
-----------------------------------------------------------------------------

-- reserves a register for the data pointer (DP)
regDP = regA

-- reserves registers for the free usage
userRegs = [regB, regC, regD, regE, regF]

-- initializes the data pointer (DP)
initDP :: [Instruction]
initDP = [Load (ImmValue 0) regDP]

-- occupies a free register
occupyReg :: Context -> (RegAddr, Context)
occupyReg (Ctx s v p (r:rs)) = (r, Ctx s v p rs)

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

-- reverses boolean value in a register
notBool :: Context -> RegAddr -> [Instruction]
notBool ctx reg = [Compute Equal reg reg0 reg]

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
       loadVar  ctx2 name reg2 0
    ++ addImm   ctx2 val  reg2 
    ++ putVar   ctx2 name reg2 0

-- loads from memory with an offset
loadAI :: Context -> RegAddr -> Offset -> RegAddr -> [Instruction]
loadAI ctx reg1 offset reg2 = 
    [
        loadImm offset reg2,
        Compute Add reg1 reg2 reg2,
        Load (IndAddr reg2) reg2
    ]

-- stores to memory with an offset
storeAI :: Context -> RegAddr -> RegAddr -> Offset -> [Instruction]
storeAI ctx reg1 reg2 offset =
    [
        loadImm offset reg3,
        Compute Add reg3 reg2 reg3,
        Store reg1 (IndAddr reg3)
    ]
    where reg3 = findReg ctx

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
loadVar :: Context -> VarName -> RegAddr -> Integer -> [Instruction]
loadVar ctx name reg idx = applyVar ctx name 
    $ \ctx regDP offset -> loadAI ctx regDP (offset + idx) reg

-- updates a variable from a register
putVar :: Context -> VarName -> RegAddr -> Integer -> [Instruction]
putVar ctx name reg idx = applyVar ctx name
    $ \ctx regDP offset -> storeAI ctx reg regDP (offset + idx)

-- update an array with values
putArrImm :: Context -> VarName -> [Integer] -> [Instruction]
putArrImm ctx name vals = applyVar ctx name
    $ \ctx regDP offset -> let 

        (reg2, ctx2) = occupyReg ctx
        valToMem idx = loadImm (vals !! idx) reg2
                     : storeAI ctx2 reg2 regDP (offset + toInteger idx)
        
        in foldl1 (++) $ map valToMem [0..length vals - 1]

applyVar :: Context -> VarName -> (Context -> RegAddr -> Offset -> [Instruction]) -> [Instruction]
applyVar ctx name apply = dpToReg ++ apply ctx2 reg2 offset
    where
        (depth, offset) = locateVar ctx name
        (reg2, ctx2)    = occupyReg ctx
        dpToReg         = loadDP ctx2 depth reg2

-- loads a data pointer at the given scope depth
loadDP :: Context -> Depth -> RegAddr -> [Instruction]
loadDP ctx depth reg = 
    [loadImm dpOffset reg, Compute Add reg regDP reg]
    where 
        (scope, scopeDepth) = ctxScope ctx
        dpOffset = - (scopeOffset ctx scope $ scopeDepth - depth)

-- finds a variable position in memory
locateVar :: Context -> VarName -> VarPos
locateVar (Ctx (scopeId, _) scopeMap _ _) name = varPos
    where 
        (varMap, _, _) = fromJust $ Map.lookup scopeId scopeMap
        (varPos, _)    = fromJust $ Map.lookup name varMap

-----------------------------------------------------------------------------
-- scope management
-----------------------------------------------------------------------------

-- returns offset from the scope at a given depth 
scopeOffset :: Context -> ScopeID -> Depth -> Offset
scopeOffset _ _ 0 = 0
scopeOffset ctx scope depthDiff = 
    scopeOffset ctx prevScope (depthDiff - 1) + fromInteger scopeSize
    where 
        (_, _, prevScope) = fromJust $ Map.lookup scope $ scopeMap ctx
        (_, scopeSize, _) = fromJust $ Map.lookup prevScope $ scopeMap ctx

-- puts instructions inside a scope
putInScope :: Context -> [Instruction] -> [Instruction]
putInScope ctx body = (updateDP ctx 1) ++ body ++ (updateDP ctx $ -1)

-- updates register with a data pointer
updateDP :: Context -> Integer -> [Instruction]
updateDP ctx@(Ctx (scope, _) scopeMap _ _) mult =
    [loadImm offsetDP reg, Compute Add reg regDP regDP]
    where 
        (_, scopeSize, _) = fromJust $ Map.lookup scope scopeMap
        offsetDP = toInteger mult * scopeSize
        reg = findReg ctx

-- builds a map with scope info (e.g. scope size, variable positions, etc.)
toScopeMap :: Map ScopeID ScopeID -> Map ScopeID VarMap -> ScopeMap
toScopeMap pathMap varMap  = scopeMap
    where 
        getInfo id varMap  = (varMap, scopeSize varMap, prevScope id)
        scopeSize varMap   = sumSeconds $ Map.elems varMap
        prevScope id       = fromMaybe (-1) (Map.lookup id pathMap)
        scopeMap           = tryInsertMap 0 (Map.empty, 0, 0)
                             $ Map.mapWithKey getInfo varMap

-- updates context for the next scope
inScopeCtx :: Context -> Context
inScopeCtx = applyScope (\(id, depth) -> (id + 1, depth + 1))

-- updates context for the next "else" scope
inScopeCtxElse :: Context -> Context
inScopeCtxElse ctx = applyScope apply ctx
    where 
        (id, _) = ctxScope ctx
        nextId  = nextNum (id + 1) (childScopes id $ scopePath ctx)
        apply (_, depth) = (nextId, depth + 1)

-- finds scopes directly contained within the given scope 
childScopes :: ScopeID -> ScopePath -> [ScopeID]
childScopes _ [] = []
childScopes p ((cx,px):xs)
    | p == px = cx : childScopes p xs
    | otherwise = childScopes p xs

-- updates the current scope
applyScope :: (Scope -> Scope) -> Context -> Context
applyScope apply (Ctx scope a b c) = Ctx (apply scope) a b c

-----------------------------------------------------------------------------
-- utilities
-----------------------------------------------------------------------------

-- sums 2nd elements of a tuple array
sumSeconds :: [(a, Integer)] -> Integer
sumSeconds [] = 0
sumSeconds ((_, x):xs) = x + sumSeconds xs 

-- finds the next number in a sorted array
nextNum :: Integer -> [Integer] -> Integer
nextNum x [] = error $ "no next number: " ++ show x 
nextNum x (y:ys) = if x < y then y else nextNum x ys 

-- inserts into a map if the key does not exist
tryInsertMap :: Ord a => a -> b -> Map a b -> Map a b
tryInsertMap k v m = if Map.member k m then m else Map.insert k v m