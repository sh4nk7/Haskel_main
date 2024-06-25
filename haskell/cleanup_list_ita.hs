-- Importazione dei moduli necessari per manipolazione di bit, liste, tempo POSIX,
-- tipi di dati numerici specifici e operazioni di input/output.
import Data.Array
import Data.Bits (shiftL, shiftR, xor)
import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)
import System.IO

-- Definizione di Rng32 come tipo Word32, utilizzato per il generatore di numeri casuali.
type Rng32 = Word32

-- Funzione per il generatore di numeri casuali XORShift32.
xorshift32 :: Rng32 -> Rng32
xorshift32 a = d
  where
    b = a `xor` (a `shiftL` 13)  -- Primo shift e XOR
    c = b `xor` (b `shiftR` 17)  -- Secondo shift e XOR
    d = c `xor` (c `shiftL` 5)   -- Terzo shift e XOR

-- Genera un numero casuale tra nmin e nmax usando il generatore.
randint :: (Int, Int) -> Rng32 -> (Int, Rng32)
randint (nmin, nmax) gen = (val, nxt)
  where
    nxt = xorshift32 gen  -- Aggiorna il generatore
    val = nmin + (fromIntegral nxt) `mod` (nmax + 1 - nmin)

-- Genera una lista infinita di numeri casuali dati un range e un generatore.
randints :: (Int, Int) -> Rng32 -> [Int]
randints range gen = val : randints range nxt
  where
    (val, nxt) = randint range gen

-- Ottiene un generatore di numeri casuali a partire dal tempo corrente.
getRng32 :: IO Rng32
getRng32 = do
  now <- getPOSIXTime
  return (round (now * 1000) :: Rng32)

-- Divide una lista in blocchi di dimensione n, utilizzato per manipolare la matrice.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Controlla se l'indice è all'interno dei limiti della matrice (non utilizzato attualmente).
indexInMatrix :: Int -> Int -> Int -> Bool
indexInMatrix r c i = (x >= 0 && x < c) && (y >= 0 && y < r)
  where
    (y, x) = divMod i c

-- Determina se una cella è adiacente a quella selezionata.
isAdjacent :: Int -> Int -> Int -> Bool
isAdjacent index cellIndex cols = dx + dy == 1
  where
    dx = abs (x - iX)  -- Differenza assoluta delle colonne
    dy = abs (y - iY)  -- Differenza assoluta delle righe
    (y, x) = divMod index cols
    (iY, iX) = divMod cellIndex cols

-- Inverte il valore di una cella se è adiacente.
flipValue :: (Bool, Int) -> Int -> Int -> Bool
flipValue (val, index) cellIndex cols
  | isAdjacent index cellIndex cols = not val
  | otherwise = val

-- Aggiorna i valori delle celle adiacenti di quella specificata.
cleanUp :: (Int, Int) -> Array Int Bool -> Int -> Array Int Bool
cleanUp (w, h) mat i = mat // [(index, flipValue (mat ! index, index) i w) | index <- [0 .. (w * h) - 1]]

-- Legge le dimensioni della matrice che devono essere entrambe maggiori di zero.
readMatrixSize :: IO (Int, Int)
readMatrixSize = do
  putStrLn "Insert the width of the matrix: "
  w <- getLine
  putStrLn "Insert the height of the matrix: "
  h <- getLine
  if not (null w) && not (null h) then do
      let (width, height) = (read w :: Int, read h :: Int)
      if width <= 0 || height <= 0
        then do
          putStrLn "Size not acceptable!"
          readMatrixSize
        else return (width, height)
    else do
      putStrLn "Please insert the width and the height of the matrix"
      readMatrixSize

-- Legge il numero di mosse casuali iniziali da effettuare (almeno 1 e al massimo la dimensione della matrice).
readNumberOfMoves :: Int -> IO Int
readNumberOfMoves size = do
  putStrLn ("Insert how many random moves I have to do (between 1 and " ++ show size ++ "): ")
  m <- getLine
  let moves = read m :: Int
  if moves <= 0 || moves > size
    then do
      putStrLn "Number of moves not acceptable!"
      readNumberOfMoves size
    else return moves

-- Genera un certo numero di mosse casuali distinte.
generateCasualMoves :: Rng32 -> Int -> Int -> [Int] -> IO [Int]
generateCasualMoves _ 0 _ acc = return (reverse acc)
generateCasualMoves gen moves size acc = do
  let (move, g) = randint (0, size - 1) gen
  if (not (move `elem` acc))
    then generateCasualMoves g (moves - 1) size (move : acc)
    else generateCasualMoves g moves size acc

-- Stampa la matrice usando simboli (O = True, X = False).
printMatrix :: (Int, Int) -> Array Int Bool -> IO ()
printMatrix (w, h) matrix = do
  putStrLn "\\nMatrix:"
  let indices = [0 .. (w * h) - 1]
      rows = chunksOf w indices -- divide the indices in rows of w elements
      rowStrings = map (\row -> unwords $ map (\i -> if matrix ! i then "O" else "X") row) rows

      matrixString = unlines rowStrings -- all the row strings are put together
  putStrLn matrixString

-- Permette all'utente di giocare al gioco.
playGame :: (Int, Int) -> Array Int Bool -> Int -> IO ()
playGame (w, h) matrix moves = do
  if all (== False) matrix
    then do
      putStrLn ("\\nCongratulations, you have solved the game in " ++ show moves ++ " moves!")
    else do
      putStrLn "\\nInsert an index (-1 to exit): "
      i <- getLine
      let index = read i :: Int
      if index == -1
        then putStrLn ("Game exited. Moves played: " ++ show moves)
        else do
          if index < 0 || index >= (w * h)
            then do
              putStrLn "Index not valid!"
              playGame (w, h) matrix moves
            else do
              let newMat = cleanUp (w, h) matrix index
              printMatrix (w, h) newMat
              playGame (w, h) newMat (moves + 1)

cleanUpGame = do
  (width, height) <- readMatrixSize

  let matrix = listArray (0, (width * height) - 1) (replicate (width * height) False)

  let moves = readNumberOfMoves (width * height)

  let gen = getRng32
  g <- gen
  m <- moves

  let initMoves = generateCasualMoves g m (width * height) []
  iMoves <- initMoves

  let matrix' = foldl (\acc i -> cleanUp (width, height) acc i) matrix iMoves

  printMatrix (width, height) matrix'

  putStrLn ("\\nNow you have to play\\n" ++ "Legend: 0 = True, X = False\\n" ++ "Goal: obtain a matrix with all False values")

  playGame (width, height) matrix' 0