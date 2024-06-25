import System.Random
import Data.List

type Matrix = [[Bool]]
type Position = (Int, Int)

-- Funzione per invertire il valore di una cella e delle sue celle adiacenti
flipCells :: Matrix -> Int -> Int -> Matrix
flipCells matrix x y = 
    let flipCell (r, row) = 
            if r == x 
                then let flippedRow = map (\(c, val) -> if abs (c - y) <= 1 && abs (r - x) <= 1 && (c /= y || r /= x) then not val else val) (zip [0..] row) 
                     in flippedRow
                else row
    in map flipCell (zip [0..] matrix)

-- Funzione per applicare l'operazione di pulizia
cleanUp :: Matrix -> Position -> Matrix
cleanUp matrix (x, y) = flipCells matrix x y

-- Funzione per stampare la matrice
printMatrix :: Matrix -> IO ()
printMatrix = mapM_ (putStrLn . unwords . map (\b -> if b then "1" else "0"))

-- Funzione per generare una matrice pulita di dimensioni date
generateCleanMatrix :: Int -> Int -> IO Matrix
generateCleanMatrix w h = do
    gen <- newStdGen
    let matrix = replicate h $ replicate w True
    return matrix

-- Funzione per generare una lista di posizioni uniche casuali all'interno della matrice
generateRandomPositions :: Int -> Int -> Int -> IO [Position]
generateRandomPositions w h m = do
    gen <- newStdGen
    let randomPositions = take m $ nub $ zip (randomRs (0, h - 1) gen) (randomRs (0, w - 1) gen)
    return randomPositions

-- Funzione per giocare il gioco di pulizia
playGame :: Int -> Int -> Int -> Matrix -> IO ()
playGame w h m matrix = do
    -- Genera le posizioni casuali
    randomPositions <- generateRandomPositions w h m
    -- Applica i movimenti casuali
    let matrixAfterRandomMoves = foldl (\m p -> cleanUp m p) matrix randomPositions
    -- Stampa la matrice dopo i movimenti casuali
    putStrLn "Matrice iniziale dopo i movimenti casuali:"
    printMatrix matrixAfterRandomMoves
    putStrLn ""
    -- Inizia il turno dell'utente
    userMoves <- userTurn matrixAfterRandomMoves 0
    putStrLn $ "Congratulazioni! Hai risolto il puzzle in " ++ show userMoves ++ " mosse!"

-- Funzione per il turno dell'utente
userTurn :: Matrix -> Int -> IO Int
userTurn matrix moves = do
    putStrLn "Inserisci 'x y' per pulire la cella alla posizione (x, y), o 'q' per uscire:"
    input <- getLine
    if input == "q"
        then return moves
        else do
            let inputList = words input
            if length inputList /= 2
                then do
                    putStrLn "Input non valido. Inserisci due numeri interi separati da spazio."
                    userTurn matrix moves
                else do
                    let [x, y] = map read inputList
                    if x < 0 || x >= length matrix || y < 0 || y >= length (head matrix)
                        then do
                            putStrLn "Input non valido. Coordinate fuori dai limiti della matrice."
                            userTurn matrix moves
                        else if not $ matrix !! x !! y
                            then do
                                putStrLn "Cella già pulita. Scegli un'altra cella."
                                userTurn matrix moves
                            else do
                                let updatedMatrix = cleanUp matrix (x, y)
                                putStrLn "Matrice dopo il tuo movimento:"
                                printMatrix updatedMatrix
                                if all (== False) (concat updatedMatrix)
                                    then return (moves + 1)
                                    else userTurn updatedMatrix (moves + 1)

-- Funzione principale
main :: IO ()
main = do
    putStrLn "Benvenuto al gioco di pulizia!"
    putStrLn "Inserisci la larghezza della matrice:"
    w <- readLn
    putStrLn "Inserisci l'altezza della matrice:"
    h <- readLn
    putStrLn "Inserisci il numero di mosse casuali da effettuare:"
    m <- readLn
    cleanMatrix <- generateCleanMatrix w h
    playGame w h m cleanMatrix
