module Table where

type Table a b c = [(a, Row b c)]
type Row a b = [(a, b)]

findRow :: (Show a, Eq a) => a -> Table a b c -> Row b c
findRow k [] = error $ "no such key: " ++ show k
findRow k ((kx, val):xs)
    | k == kx = val
    | otherwise = findRow k xs

updateRow :: (Show a, Eq a) => a -> Row b c -> Table a b c -> Table a b c
updateRow k _ [] = error $ "no such key: " ++ show k
updateRow k toAdd (x@(kx, val):xs)
    | k == kx = (kx, val ++ toAdd) : xs
    | otherwise = x : updateRow k toAdd xs

insertCell :: Eq a => a -> b -> c -> Table a b c -> Table a b c
insertCell k1 k2 v2 [] = [(k1, [(k2, v2)])] 
insertCell k1 k2 v2 (x@(k, v):xs)
    | k == k1   = (k, (k2, v2) : v) : xs
    | otherwise = x : insertCell k1 k2 v2 xs

filterCells :: Eq a => Row a b -> Row a b -> Row a b
filterCells [] _ = []
filterCells (x@(kx, _):xs) ys
    | containsCell kx ys = filterCells xs ys
    | otherwise = x : filterCells xs ys

containsCell :: Eq a => a -> Row a b -> Bool
containsCell _ [] = False
containsCell k ((kx, _):xs)
    | k == kx = True
    | otherwise = containsCell k xs

printTable :: (Show a, Show b, Show c) => Table a b c -> IO()
printTable [] = pure ()
printTable ((k,v):xs) = do
    putStrLn $ show k ++ " " ++ printRow v
    printTable xs

printRow :: (Show a, Show b) => Row a b -> String
printRow (x:xs) = show x ++ " " ++ printRow xs
printRow [] = ""