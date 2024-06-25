foldinghaskell :: [Int] -> (Int, Int)
foldinghaskell xs = foldl process (0, 0) xs where
    process (maxChanges, rooftops) x = (maxChanges', rooftops') where
        maxSoFar = takeWhile (<= x) (tail xs)
        maxChanges' = maxChanges + if null maxSoFar || x > head maxSoFar then 1 else 0
        rooftops' = rooftops + if not (null maxSoFar) && x > maximum maxSoFar then 1 else 0
        
        
-- Test della funzione con un esempio di lista
testList :: [Int]
testList = [3, 1, 4, 5, 2, 6, 7]

-- Esegui il test e stampa i risultati
main :: IO ()
main = do
    let (maxChanges, rooftops) = computeStatistics testList
    putStrLn $ "Number of times maximum changes: " ++ show maxChanges
    putStrLn $ "Number of visible rooftops: " ++ show rooftops

