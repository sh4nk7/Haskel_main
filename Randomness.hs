
{- 
Randomness (2024-05-08) 
Risk dice
Each of two players (attacker A and defender D) rolls three dice
Dice are compared in couples
A's highest die against D's highest die... and so on
In case of equal values, D's die wins
Repeat the game N times and create a list of N integer results
A wins with 3 dice... or 2, 1, 0
Optionally, also count the occurrences of each possible result (0-3)
ghci> import Data.List
ghci> sort [3,4,2]
[2,3,4]
Possibly, solve with mapping, filtering, folding, partial application…
Possibly, generalize with respect to the number of dice each player rolls (other than three).
-}

import Data.Bits (shiftL, shiftR, xor)
import Data.List (sort)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)

type Rng32 = Word32

-- Generatore di numeri casuali XOR-Shift
xorshift32 :: Rng32 -> Rng32
xorshift32 a = d
  where
    b = a `xor` (a `shiftL` 13)
    c = b `xor` (b `shiftR` 17)
    d = c `xor` (c `shiftL` 5)

-- Genera un numero casuale nell'intervallo specificato
randint :: (Int, Int) -> Rng32 -> (Int, Rng32)
randint (nmin, nmax) gen = (val, nxt)
  where
    nxt = xorshift32 gen
    val = nmin + fromIntegral (nxt `mod` (nmax - nmin + 1))

-- Ottiene un generatore di numeri casuali basato sull'orario corrente
getRng32 :: IO Rng32
getRng32 = do
  now <- getPOSIXTime
  return (round (now * 1000) :: Rng32)

-- Lancia tre dadi e restituisce una lista ordinata in modo decrescente
launchDice :: Rng32 -> ([Int], Rng32)
launchDice gen = foldr (\_ (acc, g) -> let (d, newG) = randint (1, 6) g in (d : acc, newG)) ([], gen) [1..3]

-- Simula n partite tra attaccante e difensore
playGames :: Int -> Rng32 -> [(Int, Rng32)]
playGames n gen = foldr (\_ (acc, g) -> let (aDice, newG) = launchDice g
                                            (dDice, newG') = launchDice newG
                                            score = countWins (sort aDice) (sort dDice)
                                        in ((score, newG') : acc)) ([], gen) [1..n]

-- Conta i punti vinti dall'attaccante
countWins :: [Int] -> [Int] -> Int
countWins [] [] = 0
countWins (a:as) (d:ds)
  | a > d     = 1 + countWins as ds
  | otherwise = countWins as ds

main :: IO ()
main = do
  gen <- getRng32
  let games = playGames 10 gen  -- Cambia 10 con il numero di partite desiderato
  print games
