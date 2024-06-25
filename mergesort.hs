merge :: (Ord a)=> [a] -> [a] ->[a]
merge [] [] = []
merge x [] = []
merge [] y = []
merge (x:xs) (y:ys)
   | x < y = x:merge xs (y:ys)
   | otherwise = y:merge (x:xs) ys       --commento

