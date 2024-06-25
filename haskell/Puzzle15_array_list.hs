----GIOCO DEL 15 COMPLETO CON LIST -----

import Data.Bits (xor, shiftL, shiftR)
import Data.List (delete, elemIndex)
import System.Random (randomRIO)
import Data.Maybe (fromJust)
import System.IO (hSetEcho, stdin, hSetBuffering, BufferMode(NoBuffering))

type Seed = Int

xorshift :: Seed -> (Int, Seed)
xorshift seed = (seed', seed') where
    seed' = xor (xor (shiftL seed 13) (shiftR seed 17)) (shiftL seed 5)

shuffle :: [a] -> Seed -> ([a], Seed)
shuffle [] seed = ([], seed)
shuffle xs seed = let
    (rand, newSeed) = xorshift seed
    index = rand `mod` length xs
    picked = xs !! index
    xs' = take index xs ++ drop (index + 1) xs
    (rest, finalSeed) = shuffle xs' newSeed
    in (picked : rest, finalSeed)

type Board = [Int]

printBoard :: Board -> IO ()
printBoard board = putStrLn . unlines . map unwords $ chunksOf 4 (map show board)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

move :: Char -> Board -> Board
move direction board =
    let index = fromJust $ elemIndex 0 board
        (row, col) = (index `div` 4, index `mod` 4)
        swap i j = take (min i j) board ++ [board !! max i j] ++ take (max i j - min i j - 1) (drop (min i j + 1) board) ++ [board !! min i j] ++ drop (max i j + 1) board
    in case direction of
        'u' -> if row > 0 then swap index (index - 4) else board
        'd' -> if row < 3 then swap index (index + 4) else board
        'l' -> if col > 0 then swap index (index - 1) else board
        'r' -> if col < 3 then swap index (index + 1) else board
        _   -> board

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    seed <- randomRIO (1, 100000)
    let (shuffledBoard, _) = shuffle ([1..15] ++ [0]) seed
    gameLoop shuffledBoard where
        gameLoop board = do
            printBoard board
            if board == [1..15] ++ [0] then putStrLn "Congratulations, you won!"
            else do
                moveChar <- getChar
                gameLoop (move moveChar board)
				
----GIOCO DEL 15 COMPLETO CON ARRAY -----

import Data.Array
import Data.List (elemIndex)
import System.Random (randomRIO)
import Data.Maybe (fromJust)
import Data.Bits (xor, shiftL, shiftR)
import System.IO (hSetEcho, stdin, hSetBuffering, BufferMode(NoBuffering))

type Seed = Int

xorshift :: Seed -> (Int, Seed)
xorshift seed = (seed', seed') where
    seed' = xor (xor (shiftL seed 13) (shiftR seed 17)) (shiftL seed 5)

shuffle :: [a] -> Seed -> ([a], Seed)
shuffle [] seed = ([], seed)
shuffle xs seed = let
    (rand, newSeed) = xorshift seed
    index = rand `mod` length xs
    picked = xs !! index
    xs' = take index xs ++ drop (index + 1) xs
    (rest, finalSeed) = shuffle xs' newSeed
    in (picked : rest, finalSeed)

type Board = Array Int Int

printBoard :: Board -> IO ()
printBoard board = putStrLn . unlines . map unwords $ [ [ show (board ! (row * 4 + col)) | col <- [1..4] ] | row <- [0..3] ]

move :: Char -> Board -> Board
move direction board =
    let index = fromJust $ elemIndex 0 (elems board)
        (row, col) = (index `div` 4, index `mod` 4)
        swap i j = board // [(i, board ! j), (j, board ! i)]
    in case direction of
        'u' -> if row > 0 then swap index (index - 4) else board
        'd' -> if row < 3 then swap index (index + 4) else board
        'l' -> if col > 0 then swap index (index - 1) else board
        'r' -> if col < 3 then swap index (index + 1) else board
        _   -> board

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    seed <- randomRIO (1, 100000)
    let (shuffled, _) = shuffle ([1..15] ++ [0]) seed
        board = listArray (0, 15) shuffled
    gameLoop board where
        gameLoop board = do
            printBoard board
            if board == listArray (0, 15) ([1..15] ++ [0]) then putStrLn "Congratulations, you won!"
            else do
                moveChar <- getChar
                gameLoop (move moveChar board)

----COME ESEGUIRE ------

Considerazioni per il Funzionamento
Per far funzionare il gioco, ecco alcuni passaggi che potresti seguire:

Installazione di Haskell: Assicurati di avere installato Haskell e GHC. Puoi scaricarli dal sito ufficiale di Haskell.

Esecuzione del Codice:

Salva il codice in un file con estensione .hs, per esempio Puzzle15.hs.
Apri un terminale o un prompt dei comandi.
Naviga nella directory dove hai salvato il file.
Compila ed esegui il programma con il comando:
bash
Copia codice

runghc Puzzle15.hs

Oppure, se preferisci compilare in un eseguibile:
bash

Copia codice
ghc Puzzle15.hs -o Puzzle15
./Puzzle15

Giocare:

Durante il gioco, inserisci 'u', 'd', 'l', o 'r' per muovere le tessere rispettivamente su, giù, sinistra, o destra.
L'output sarà visibile nel terminale, e ti verrà chiesto di fare la prossima mossa finché non avrai sistemato tutte le tessere in ordine crescente.
