{- Fibonacci list in Haskell
Write a Haskell function
Define the best parameters
It has to produce an infinite list of numbers
Corresponding to the Fibonacci sequence
[0, 1, 1, 2, 3, 5, 8, 13…]
-}


fibonaccilist :: [Integer]
fibonaccilist = 0 : 1 : zipWith (+) fibonaccilist (tail fibonaccilist)
