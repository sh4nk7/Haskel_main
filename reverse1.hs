reverseList :: [a] -> [a]
reverseList list = reverseTail list []
  where
    reverseTail :: [a] -> [a] -> [a]
    reverseTail [] reversed = reversed
    reverseTail (x:xs) reversed = reverseTail xs (x:reversed)
