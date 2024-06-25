
{-
Pythagoras with Haskell
Which right triangle…
that has integers for all sides…
and all sides equal to or smaller than 10…
has a perimeter of 24?
Solve using a comprehension
-}


triangoloperimetro24 = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x + y + z == 24, z ^ 2 == x ^ 2 + y ^ 2]


