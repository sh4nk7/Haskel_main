--tris_array_xorshift--

import Data.Array
import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import System.Random (randomRIO)

type Board = Array (Int, Int) Char
type Rng32 = Word32

-- Funzione per generare una sequenza di numeri casuali utilizzando l'algoritmo XOR shift
generaNumeriCasuali :: Int -> Rng32 -> [Int]
generaNumeriCasuali n rng = take n $ map (\x -> fromIntegral (x `mod` 9)) $ iterate xorshift32 rng

-- Implementazione di XOR shift per generare numeri casuali
xorshift32 :: Rng32 -> Rng32
xorshift32 a = d where
    b = a `xor` (a `shiftL` 13)
    c = b `xor` (b `shiftR` 17)
    d = c `xor` (c `shiftL` 5)

-- Funzione per ottenere un seed Rng32 dall'orologio POSIX
getRng32 :: IO Rng32
getRng32 = fromIntegral <$> randomRIO (minBound, maxBound)

-- Funzione per creare un nuovo tabellone vuoto
nuovoTabellone :: Board
nuovoTabellone = listArray ((1,1),(3,3)) (replicate 9 ' ')

-- Funzione per stampare il tabellone di gioco
stampareTabellone :: Board -> IO ()
stampareTabellone board = do
    let ((minRow, minCol), (maxRow, maxCol)) = bounds board
    mapM_ (\row -> do
                mapM_ (\col -> putStr [board ! (row, col), ' ']) [minCol..maxCol]
                putStrLn "") [minRow..maxRow]

-- Funzione per controllare se c'è una vittoria
controllaVittoria :: Board -> Char -> Bool
controllaVittoria b c = any (all (== c)) [r | r <- rows b] || any (all (== c)) [c | c <- cols b] || all (\i -> b ! (i, i) == c) [1..3] || all (\i -> b ! (i, 4 - i) == c) [1..3]
    where
        rows b = [[b ! (i, j) | j <- [1..3]] | i <- [1..3]]
        cols b = [[b ! (i, j) | i <- [1..3]] | j <- [1..3]]

-- Funzione principale per giocare a Tris
giocaTris :: Rng32 -> IO ()
giocaTris rng = gioca nuovoTabellone 'X' rng

-- Funzione ricorsiva per giocare a Tris
gioca :: Board -> Char -> Rng32 -> IO ()
gioca board player rng = do
    putStrLn $ "Turno del giocatore " ++ [player]
    stampareTabellone board
    putStrLn "Seleziona la riga e la colonna (es. 1 2):"
    input <- getLine
    let [row, col] = map read $ words input
    let newBoard = if board ! (row, col) == ' ' then aggiornaTabellone board player (row, col) else board
    if controllaVittoria newBoard player
        then do
            putStrLn $ "Il giocatore " ++ [player] ++ " ha vinto!"
            stampareTabellone newBoard
        else if all (\pos -> newBoard ! pos /= ' ') [(i, j) | i <- [1..3], j <- [1..3]]
            then do
                putStrLn "È un pareggio!"
                stampareTabellone newBoard
            else gioca newBoard (if player == 'X' then 'O' else 'X') (xorshift32 rng)

-- Funzione per aggiornare il tabellone con la mossa del giocatore
aggiornaTabellone :: Board -> Char -> (Int, Int) -> Board
aggiornaTabellone board player (row, col) = board // [((row, col), player)]

-- Funzione principale
main :: IO ()
main = do
    putStrLn "Benvenuto nel gioco del Tris!"
    rng <- getRng32
    giocaTris rng
