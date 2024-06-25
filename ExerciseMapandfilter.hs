{-Multiples with lambdas
Define a Haskell function multipleAny x ys :: Int -> [Int] -> Bool
Is x multiple of some number in ys?
multipleAny(40, [3,5,7])?
Use or, map and a lambda
> or([False, True, False])
True
> multipleAny(40, [3,5,7])
True-}

multipleAny :: Int -> [Int] -> Bool
multipleAny x ys = or (map (\y -> x `mod` y == 0) ys)




{-Multiples without lambda
Define a Haskell function multipleAny x ys :: Int -> [Int] -> Bool
Is x multiple of some number in ys?
multipleAny(40, [3,5,7])?
Use map, without any lambda
> or([False, True, False])
True
> multipleAny(40, [3,5,7])
True-}

multipleAny :: -> [Int] -> Bool
multipleAny x ys = or (map(isMultiple x) ys)
        where 
            isMultiple :: Int -> Int -> Bool
            isMultiple x y = x `mod` y == 0 
 




{-Sum of squares in Haskell
Sum all squares that are multiple of 3 or 5 or 7 and smaller than 5,000
Laziness: map over and filter an infinite list
64107-}


squares :: [Int]
squares = map (^2) [1..]

isMultipleOf357 :: Int -> Bool
isMultipleOf357 x = any (\y -> x `mod` y == 0) [3, 5, 7]

sumOfSquares :: Int
sumOfSquares =
  let filteredSquares = filter isMultipleOf357 squares
      squaresBelow5000 = takeWhile (<5000) filteredSquares
  in sum squaresBelow5000
