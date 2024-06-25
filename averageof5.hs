{- Average of 5 elements -
Returns the average of the first 5 values (if present) of a list.
-}

avg list = (sum list) / (fromIntegral (length list))
avg5 list = avg (take 5 list)