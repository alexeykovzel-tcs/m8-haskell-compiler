module Utils.Sprockell where

import Sprockell
import Data.Maybe
import Data.Char
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

type Offset     = Integer
type Depth      = Integer

type VarName    = String
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

-- reserve register for data pointer (DP)
regDP = regA

-- registers for free usage
userRegs = [regB, regC, regD, regE, regF]

-- initializes data pointer (DP)
initDP :: [Instruction]
initDP = [Load (ImmValue 0) regDP]

-- occupies free register
occupyReg :: Context -> (RegAddr, Context)
occupyReg (Ctx s v p (r:rs)) = (r, Ctx s v p rs)

-- finds free register
findReg :: Context -> RegAddr
findReg ctx = let (r:_) = freeRegs ctx in r

-- copies value from register 1 to register 2
copyReg :: RegAddr -> RegAddr -> Instruction
copyReg reg1 reg2 = Compute Add reg0 reg1 reg2

-- loads value to register
loadImm :: Integer -> RegAddr -> Instruction
loadImm val reg = Load (ImmValue $ fromInteger val) reg

-- adds value to register
addImm :: Context -> Integer -> RegAddr -> [Instruction]
addImm ctx val reg = let reg2 = findReg ctx in 
    [loadImm val reg2, Compute Add reg reg2 reg]

-- reverses boolean value in register
notBool :: Context -> RegAddr -> [Instruction]
notBool ctx reg = [Compute Equal reg reg0 reg]

-----------------------------------------------------------------------------
-- memory instructions
-----------------------------------------------------------------------------

-- increments variable in memory
incrMem :: Context -> VarName -> [Instruction]
incrMem ctx name = addMem ctx name 1

-- adds value to variable in memory
addMem :: Context -> VarName -> Integer -> [Instruction]
addMem ctx name val = let (reg2, ctx2) = occupyReg ctx in
       loadVar    ctx2 name reg2
    ++ addImm     ctx2 val  reg2 
    ++ updateVar  ctx2 name reg2

-- loads from memory with offset
offsetLoad :: Context -> RegAddr -> Offset -> RegAddr -> [Instruction]
offsetLoad ctx reg1 offset reg2 = 
    [
        loadImm offset reg2,
        Compute Add reg1 reg2 reg2,
        Load (IndAddr reg2) reg2
    ]

-- stores to memory with offset
offsetStore :: Context -> RegAddr -> RegAddr -> Offset -> [Instruction]
offsetStore ctx reg1 reg2 offset =
    [
        loadImm offset reg3,
        Compute Add reg3 reg2 reg2,
        Store reg1 (IndAddr reg2)
    ]
    where reg3 = findReg ctx

-----------------------------------------------------------------------------
-- IO instructions
-----------------------------------------------------------------------------

-- prints a string
writeString :: Context -> String -> [Instruction]
writeString ctx str = concat $ map (writeChar $ findReg ctx) str

-- prints a single character
writeChar :: RegAddr -> Char -> [Instruction]
writeChar reg c = [Load (ImmValue $ ord c) reg, WriteInstr reg charIO]

-----------------------------------------------------------------------------
-- variable instructions
-----------------------------------------------------------------------------

-- loads variable data from memory to register
loadVar :: Context -> VarName -> RegAddr -> [Instruction]
loadVar ctx name reg = dpToReg ++ valToReg
    where 
        (depth, offset) = locateVar ctx name
        (reg2, ctx2)    = occupyReg ctx
        dpToReg         = loadDP ctx2 depth reg2
        valToReg        = offsetLoad ctx2 reg2 offset reg

-- updates variable in memory from register
updateVar :: Context -> VarName -> RegAddr -> [Instruction]
updateVar ctx name reg = dpToReg ++ varToMem
    where
        (depth, offset) = locateVar ctx name
        (reg2, ctx2)    = occupyReg ctx
        dpToReg         = loadDP ctx2 depth reg2
        varToMem        = offsetStore ctx2 reg reg2 offset

-- updates variable in memory by value
updateVarImm :: Context -> VarName -> Integer -> [Instruction]
updateVarImm ctx name val = 
    (loadImm val reg2) : (updateVar ctx2 name reg2)
    where (reg2, ctx2) = occupyReg ctx

-- loads data pointer at scope depth
loadDP :: Context -> Depth -> RegAddr -> [Instruction]
loadDP ctx depth reg = 
    [loadImm dpOffset reg, Compute Add reg regDP reg]
    where 
        (scope, scopeDepth) = ctxScope ctx
        depthDiff = scopeDepth - depth
        dpOffset = - (scopeOffset ctx scope depthDiff)

-- finds variable position in memory
locateVar :: Context -> VarName -> VarPos
locateVar (Ctx (scopeId, _) scopeMap _ _) name = varPos
    where 
        (varMap, _, _) = fromJust $ Map.lookup scopeId scopeMap
        (varPos, _)    = fromJust $ Map.lookup name varMap

-----------------------------------------------------------------------------
-- scope instructions
-----------------------------------------------------------------------------

-- returns offset from scope at given depth 
scopeOffset :: Context -> ScopeID -> Depth -> Offset
scopeOffset _ _ 0 = 0
scopeOffset ctx scope depthDiff = 
    scopeOffset ctx prevScope (depthDiff - 1) + prevScopeSize
    where 
        prevScope     = getPrevScope ctx scope
        prevScopeSize = fromInteger $ getScopeSize ctx prevScope

-- returns previous scope id by id
getPrevScope :: Context -> ScopeID -> ScopeID
getPrevScope ctx scope = prevScope
    where (_, _, prevScope) = getScopeInfo ctx scope 

-- returns scope size by id
getScopeSize :: Context -> ScopeID -> ScopeSize
getScopeSize ctx scope = scopeSize
    where (_, scopeSize, _) = getScopeInfo ctx scope 

-- returns scope info by id
getScopeInfo :: Context -> ScopeID -> ScopeInfo
getScopeInfo ctx scope = fromJust $ Map.lookup scope $ scopeMap ctx

-- puts body instructions inside a scope
inScope :: Context -> [Instruction] -> [Instruction]
inScope ctx body = (updateDP ctx 1) ++ body ++ (updateDP ctx $ -1)

-- updates register with a data pointer
updateDP :: Context -> Integer -> [Instruction]
updateDP ctx@(Ctx (scope, _) scopeMap _ _) mult =
    [loadImm offsetDP reg, Compute Add reg regDP regDP]
    where 
        (_, scopeSize, _) = fromJust $ Map.lookup scope scopeMap
        offsetDP = toInteger mult * scopeSize
        reg = findReg ctx

-- finishes building a map with scope info (scope size, variable positions, etc.)
toScopeMap :: Map.Map ScopeID ScopeID -> Map.Map ScopeID VarMap -> ScopeMap
toScopeMap pathMap varMap = Map.mapWithKey getInfo varMap
    where 
        getInfo id varMap  = (varMap, scopeSize varMap, prevScope id)
        scopeSize varMap   = sumSeconds $ Map.elems varMap
        prevScope id       = fromMaybe (-1) (Map.lookup id pathMap)

-- updates context for the next scope
inScopeCtx :: Context -> Context
inScopeCtx = applyScope (\(id, depth) -> (id + 1, depth + 1))

-- applies a function to the current scope
applyScope :: (Scope -> Scope) -> Context -> Context
applyScope apply (Ctx scope a b c) = Ctx (apply scope) a b c

-----------------------------------------------------------------------------
-- ugly code to find "else" context 

elseCtx :: Context -> Context
elseCtx ctx = applyScope (\(_, depth) -> (nextId, depth + 1)) ctx
    where 
        nextId = scopeNext (id + 1) id $ scopePath ctx 
        (id, _) = ctxScope ctx

scopeNext :: ScopeID -> ScopeID -> ScopePath -> ScopeID
scopeNext c p path = nextNum c $ sort $ childScopes p path

childScopes :: ScopeID -> ScopePath -> [ScopeID]
childScopes _ [] = []
childScopes p ((cx,px):xs)
    | p == px = cx : childScopes p xs
    | otherwise = childScopes p xs

-----------------------------------------------------------------------------
-- utilities
-----------------------------------------------------------------------------

sumSeconds :: [(a, Integer)] -> Integer
sumSeconds [] = 0
sumSeconds ((_, x):xs) = x + sumSeconds xs 

nextNum :: Integer -> [Integer] -> Integer
nextNum x [] = error $ "no next number: " ++ show x 
nextNum x (y:ys)
    | x < y = y
    | otherwise = nextNum x ys 