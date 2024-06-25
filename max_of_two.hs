{-
Max of two values
Write a Haskell maxOfTwo function, with guards
Two parameters
Return the maximum of the two
Generic parameters
What's the needed type constraint?
-}

maxOfTwo :: (Ord a) => a -> a -> a
maxOfTwo a b
  | a < b = b
  | otherwise = a