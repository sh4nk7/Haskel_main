{-
Merge sort
Define a merge function in Haskell
Takes two sorted lists
Returns a sorted list, with all the elems
Define a mergeSort function in Haskell
Takes a list
Splits it at half
Sorts each part, recursively
Merges the two sorted parts
In the figure: split is red, merge is green.
-}

-Define a merge function in Haskell Takes two sorted lists Returns a sorted list, with all the elems

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys


- - Halve - Splits a list into two halves

halve :: [a] -> ([a], [a])
halve l = (take half l, drop half l)
        where half = (length l `div` 2)


- Define a mergeSort function in Haskell Takes a list Splits it at half Sorts each part, recursively Merges the two sorted parts

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort list = merge (mergesort left) (mergesort right)
  where (left, right) = halve list