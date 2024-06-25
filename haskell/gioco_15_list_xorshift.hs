---gioco del 15 con list ------

import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.List.Split (chunksOf)
import Data.List (tails, elemIndex)
import System.Random (randomRIO)

type Board = [[Int]]
type Rng32 = Word32

-- Funzione per generare una sequenza di numeri casuali utilizzando l'algoritmo XOR shift
generaNumeriCasuali :: Int -> Rng32 -> [Int]
generaNumeriCasuali n rng = take n $ map (\x -> fromIntegral (x `mod` 16)) $ iterate xorshift32 rng

-- Funzione per generare una nuova disposizione casuale delle tessere utilizzando numeri casuali
generaDisposizioneCasuale :: IO Board
generaDisposizioneCasuale = do
    rng <- getRng32
    let nums = generaNumeriCasuali 16 rng
        board = chunksOf 4 nums
    return board

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

-- Funzione per ottenere un seed Rng32 dall'orologio POSIX
getRng32 :: IO Rng32
getRng32 = do
    now <- round <$> getPOSIXTime
    return (fromIntegral (now `mod` maxBound) :: Rng32)

-- Funzione per controllare se la disposizione è risolvibile
disposizioneRisolvibile :: Board -> Bool
disposizioneRisolvibile board = 
    let flattenBoard = concat board
        inversions = length [(x, y) | (x:ys) <- tails flattenBoard, y <- ys, x /= 0, y /= 0, x > y]
        emptyRow = 1 + div (elemIndex 0 flattenBoard) 4
    in even inversions == even emptyRow

-- Funzione principale per avviare il gioco
main :: IO ()
main = do
    putStrLn "Benvenuto nel Gioco del 15!"
    board <- generaDisposizioneCasuale
    if not (disposizioneRisolvibile board)
        then do
            putStrLn "La disposizione generata non è risolvibile. Genera una nuova disposizione."
            main
        else do
            putStrLn "Disposizione iniziale:"
            mapM_ print board
