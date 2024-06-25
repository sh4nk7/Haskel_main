{-
Folding in Haskell
Given a list of unique positive integers values...
How many times does the maximum change?
Can you compute it with a single fold?
From left to right, how many “rooftops” are visible? (In the example: 4)
-}




foldinghaskell :: [Int] -> (Int, Int)
foldinghaskell xs = foldl process (0, 0) xs where
    process (maxChanges, rooftops) x = (maxChanges', rooftops') where
        maxSoFar = takeWhile (<= x) (tail xs)
        maxChanges' = maxChanges + if null maxSoFar || x > head maxSoFar then 1 else 0
        rooftops' = rooftops + if not (null maxSoFar) && x > maximum maxSoFar then 1 else 0