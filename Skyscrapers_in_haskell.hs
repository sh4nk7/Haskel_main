{-Esercizio Maximum with tail recursion-}

maxTailRec :: Ord a => [a] -> a -> a
maxTailRec [] acc = acc  
maxTailRec (x:xs) acc = maxTailRec xs (max x acc) 



maxInList :: Ord a => [a] -> a
maxInList (x:xs) = maxTailRec xs x 
