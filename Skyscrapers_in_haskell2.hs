countMaxChanges :: [Int] -> Int -> Int -> (Int, Int)
countMaxChanges [] highest count = (highest, count)
countMaxChanges (x:xs) highest count
    | x > highest = countMaxChanges xs x (count + 1)
    | otherwise   = countMaxChanges xs highest count

countVisibleRooftops :: [Int] -> (Int, Int)
countVisibleRooftops (x:xs) = countMaxChanges xs x 1
