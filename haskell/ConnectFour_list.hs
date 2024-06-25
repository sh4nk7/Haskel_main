import Data.Bits
import Data.List
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad (foldM)

type Board = [[Maybe Bool]]
type Column = Int
type Player = Bool  -- True rappresenta un giocatore, False l'altro

-- XORShift algorithm for pseudo-random number generation
xorshift :: Int -> Int
xorshift x = x `xor` (x `shiftL` 13) `xor` (x `shiftR` 17) `xor` (x `shiftL` 5)

-- Initialize an empty board
initBoard :: Board
initBoard = replicate 6 (replicate 7 Nothing)

-- Make a move on the board at the specified column
makeMove :: Board -> Column -> Player -> Maybe Board
makeMove board col player =
    if col < 0 || col >= 7 then Nothing
    else let (newBoard, valid) = runColumn 5 board
         in if valid then Just newBoard else Nothing
  where
    runColumn :: Int -> Board -> (Board, Bool)
    runColumn (-1) b = (b, False)
    runColumn row b = case (b !! row) !! col of
        Just _ -> runColumn (row - 1) b
        Nothing -> (take row b ++ [replaceNth col (Just player) (b !! row)] ++ drop (row + 1) b, True)

    replaceNth :: Int -> a -> [a] -> [a]
    replaceNth n newVal (x:xs)
        | n == 0 = newVal : xs
        | otherwise = x : replaceNth (n - 1) newVal xs
    replaceNth _ _ [] = []

-- Check if a player has won
checkWin :: Board -> Player -> Bool
checkWin board player = any isWinningLine (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = diagonals board

    isWinningLine :: [Maybe Bool] -> Bool
    isWinningLine line = any (>= 4) . map length . filter (== replicate 4 (Just player)) . group $ line

    diagonals :: [[Maybe Bool]] -> [[Maybe Bool]]
    diagonals b = [diag b (r, 0) | r <- [0..5]] ++ [diag b (0, c) | c <- [1..6]] ++
                  [diag (reverse b) (r, 0) | r <- [0..5]] ++ [diag (reverse b) (0, c) | c <- [1..6]]

    diag :: [[Maybe Bool]] -> (Int, Int) -> [Maybe Bool]
    diag b (r, c)
        | inBounds (r, c) = (b !! r) !! c : diag b (r+1, c+1)
        | otherwise = []

    inBounds :: (Int, Int) -> Bool
    inBounds (r, c) = r >= 0 && r < 6 && c >= 0 && c < 7

-- Generate a random move using XORShift
randomMove :: Board -> Int -> (Board, Int)
randomMove board seed =
    let col = abs (xorshift seed) `mod` 7
    in case makeMove board col True of
        Just newBoard -> (newBoard, xorshift seed)
        Nothing -> randomMove board (xorshift seed)

main :: IO ()
main = do
    let seed = 12345  -- Initial seed for pseudo-random number generator
    let board = initBoard
    let (finalBoard, _) = randomMove board seed
    putStrLn "Final board after one random move:"
    print finalBoard
