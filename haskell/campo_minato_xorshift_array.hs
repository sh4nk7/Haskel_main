import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Array

type Rng32 = Word32
type Grid = Array (Int, Int) Char

-- Crea una griglia vuota
emptyGrid :: (Int, Int) -> Grid
emptyGrid (rows, cols) = array ((0,0),(rows-1,cols-1)) [((r,c),' ') | r <- [0..rows-1], c <- [0..cols-1]]

-- Aggiunge bombe casuali alla griglia
addMines :: Int -> (Int, Int) -> Rng32 -> Grid -> Grid
addMines 0 _ _ grid = grid
addMines n (rows, cols) rng grid = addMines (n-1) (rows, cols) nextRng newGrid
    where
        (r, nextRng1) = randInt (0, rows - 1) rng
        (c, nextRng2) = randInt (0, cols - 1) nextRng1
        newGrid = grid // [((r,c),'*')]
        nextRng = xorshift32 nextRng2

-- Controlla se le coordinate sono all'interno della griglia
isValid :: (Int, Int) -> (Int, Int) -> Bool
isValid (r, c) (rows, cols) = r >= 0 && r < rows && c >= 0 && c < cols

-- Calcola il numero di bombe adiacenti a una cella
countAdjacentMines :: (Int, Int) -> Grid -> Int
countAdjacentMines (r, c) grid =
    length $ filter (== '*') [grid ! (nr, nc) | (nr, nc) <- neighbors]
    where
        ((minRow, minCol), (maxRow, maxCol)) = bounds grid
        neighbors = [(nr, nc) | nr <- [r - 1 .. r + 1],
                                nc <- [c - 1 .. c + 1],
                                isValid (nr, nc) ((maxRow+1),(maxCol+1)),
                                (nr, nc) /= (r, c)]

-- Mostra la griglia
showGrid :: Grid -> String
showGrid grid = unlines [concat [grid ! (r, c) : " " | c <- [minCol..maxCol]] | r <- [minRow..maxRow]]
    where ((minRow, minCol), (maxRow, maxCol)) = bounds grid

-- Funzione principale per generare il gioco Campo Minato
generateMinesweeper :: Int -> Int -> Int -> IO ()
generateMinesweeper rows cols numMines = do
    rng <- getRng32
    let grid = addMines numMines (rows, cols) rng (emptyGrid (rows, cols))
        solvedGrid = array ((0,0),(rows-1,cols-1)) [((r,c), if grid ! (r,c) == '*' then '*' else
                      head (show (countAdjacentMines (r, c) grid)))
                      | r <- [0..rows-1], c <- [0..cols-1]]
    putStrLn (showGrid solvedGrid)

-- Funzione per generare un numero casuale intero in un intervallo
randInt :: (Int, Int) -> Rng32 -> (Int, Rng32)
randInt (nmin, nmax) gen = (val, nxt) where
    nxt = xorshift32 gen
    val = nmin + fromIntegral (nxt `mod` fromIntegral (nmax + 1 - nmin))

-- Implementazione di XOR shift per generare numeri casuali
xorshift32 :: Rng32 -> Rng32
xorshift32 a = d where
    b = a `xor` (a `shiftL` 13)
    c = b `xor` (b `shiftR` 17)
    d = c `xor` (c `shiftL` 5)

main :: IO ()
main = do
    putStrLn "Generazione Campo Minato..."
    generateMinesweeper 8 8 10
	
--In questa versione, la griglia è rappresentata come un array, dove l'indice (r, c) corrisponde alla riga r e alla colonna c. La funzione emptyGrid --crea una griglia vuota di dimensioni specificate, mentre addMines aggiunge bombe casuali alla griglia utilizzando gli array. Tutte le altre funzioni --sono state adattate di conseguenza per utilizzare array invece di liste.
