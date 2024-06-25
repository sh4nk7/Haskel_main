--tris_list_xorshift--

import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.List (intersperse)
import System.Random (randomRIO)

type Board = [[Char]]  -- Rappresentazione del tabellone
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
nuovoTabellone = replicate 3 (replicate 3 ' ')

-- Funzione per stampare il tabellone di gioco
stampareTabellone :: Board -> IO ()
stampareTabellone = mapM_ (putStrLn . intersperse '|' . map (\c -> if c == ' ' then '_' else c))

-- Funzione per controllare se c'è una vittoria
controllaVittoria :: Board -> Char -> Bool
controllaVittoria b c = any (all (== c)) [r | r <- b] || any (all (== c)) [c | c <- transpose b] || all (\(i, j) -> b !! i !! j == c) [(i, i) | i <- [0..2]] || all (\(i, j) -> b !! i !! j == c) [(i, 2 - i) | i <- [0..2]]

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
    let newBoard = if board !! (row - 1) !! (col - 1) == ' ' then aggiornaTabellone board player (row - 1) (col - 1) else board
    if controllaVittoria newBoard player
        then do
            putStrLn $ "Il giocatore " ++ [player] ++ " ha vinto!"
            stampareTabellone newBoard
        else if all (\row -> all (/= ' ') row) newBoard
            then do
                putStrLn "È un pareggio!"
                stampareTabellone newBoard
            else gioca newBoard (if player == 'X' then 'O' else 'X') (xorshift32 rng)

-- Funzione per aggiornare il tabellone con la mossa del giocatore
aggiornaTabellone :: Board -> Char -> Int -> Int -> Board
aggiornaTabellone board player row col = take row board ++ [take col (board !! row) ++ [player] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

-- Funzione principale
main :: IO ()
main = do
    putStrLn "Benvenuto nel gioco del Tris!"
    rng <- getRng32
    giocaTris rng
