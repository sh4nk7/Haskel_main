
--Per creare un gioco Campo Minato, puoi utilizzare il generatore di numeri casuali fornito (randints) per posizionare le bombe in posizioni casuali su --una griglia e quindi calcolare il numero di bombe adiacenti --a ciascuna cella. Ecco una possibile implementazione:

import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (intersperse)

type Rng32 = Word32
type Grid = [[Char]]

-- Genera una griglia vuota
emptyGrid :: Int -> Int -> Grid
emptyGrid rows cols = replicate rows (replicate cols ' ')

-- Aggiunge bombe casuali alla griglia
addMines :: Int -> (Int, Int) -> Rng32 -> Grid -> Grid
addMines 0 _ _ grid = grid
addMines n (rows, cols) rng grid =
    addMines (n-1) (rows, cols) nextRng newGrid
    where
        (r, nextRng1) = randInt (0, rows - 1) rng
        (c, nextRng2) = randInt (0, cols - 1) nextRng1
        newGrid = updateGrid (r, c) '*' grid
        nextRng = xorshift32 nextRng2

-- Aggiorna la griglia con una nuova cella
updateGrid :: (Int, Int) -> Char -> Grid -> Grid
updateGrid (r, c) ch grid = take r grid ++
                            [take c (grid !! r) ++ [ch] ++ drop (c + 1) (grid !! r)] ++
                            drop (r + 1) grid

-- Controlla se le coordinate sono all'interno della griglia
isValid :: (Int, Int) -> Int -> Int -> Bool
isValid (r, c) rows cols = r >= 0 && r < rows && c >= 0 && c < cols

-- Calcola il numero di bombe adiacenti a una cella
countAdjacentMines :: (Int, Int) -> Grid -> Int
countAdjacentMines (r, c) grid =
    length $ filter (== '*') [grid !! nr !! nc | (nr, nc) <- neighbors]
    where
        rows = length grid
        cols = length (head grid)
        neighbors = [(nr, nc) | nr <- [r - 1 .. r + 1],
                                nc <- [c - 1 .. c + 1],
                                isValid (nr, nc) rows cols,
                                (nr, nc) /= (r, c)]

-- Mostra la griglia
showGrid :: Grid -> String
showGrid grid = unlines (map (intersperse ' ') grid)

-- Funzione principale per generare il gioco Campo Minato
generateMinesweeper :: Int -> Int -> Int -> IO ()
generateMinesweeper rows cols numMines = do
    rng <- getRng32
    let grid = addMines numMines (rows, cols) rng (emptyGrid rows cols)
        solvedGrid = [[if cell == '*' then '*' else
                      head (show (countAdjacentMines (r, c) grid))
                      | (c, cell) <- zip [0..] row]
                      | (r, row) <- zip [0..] grid]
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


--Questo codice genera un Campo Minato 8x8 con 10 mine. Puoi personalizzare le dimensioni della griglia e il numero di mine passando i valori --appropriati alla funzione generateMinesweeper. Spero che questo ti dia una buona base da cui iniziare! Se hai domande o hai bisogno di ulteriori --chiarimenti, non esitare a chiedere.