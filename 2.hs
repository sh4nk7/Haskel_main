maxOfTwo :: (Ord a) => a -> a -> a
maxOfTwo a b
  | a < b = b
  | otherwise = a

