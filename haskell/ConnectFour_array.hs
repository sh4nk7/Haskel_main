import System.IO (hFlush, stdout)
import Data.Bits
import Data.Array
import Control.Monad (foldM)

type Board = Array (Int, Int) (Maybe Bool)
type Column = Int
type Player = Bool  -- True rappresenta un giocatore, False l'altro

-- XORShift algorithm for pseudo-random number generation
xorshift :: Int -> Int
xorshift x = x `xor` (x `shiftL` 13) `xor` (x `shiftR` 17) `xor` (x `shiftL` 5)

-- Initialize an empty board
initBoard :: Board
initBoard = array ((0, 0), (5, 6)) [((r, c), Nothing) | r <- [0..5], c <- [0..6]]

-- Make a move on the board at the specified column
makeMove :: Board -> Column -> Player -> Maybe Board
makeMove board col player =
    if col < 0 || col > 6 then Nothing
    else case foldl (updateBoard col) Nothing [0..5] of
         Just newBoard -> Just newBoard
         Nothing -> Nothing
  where
    updateBoard :: Column -> Maybe Board -> Int -> Maybe Board
    updateBoard _ (Just b) _ = Just b
    updateBoard c Nothing row = 
      if isNothing (board ! (row, c))
      then Just (board // [((row, c), Just player)])
      else Nothing

-- Check if a player has won
checkWin :: Board -> Player -> Bool
checkWin board player = any isWinning [rows, cols, diags1, diags2]
  where
    rows = [[board ! (r, c) | c <- [0..6]] | r <- [0..5]]
    cols = [[board ! (r, c) | r <- [0..5]] | c <- [0..6]]
    diags1 = [ [board ! (r+i, c+i) | i <- [0..min (5-r) (6-c)]] | r <- [0..5], c <- [0..6], r + c <= 6 ]
    diags2 = [ [board ! (r+i, c-i) | i <- [0..min (5-r) c]] | r <- [0..5], c <- [0..6], r <= c + 5 ]
    isWinning lineList = any (>= 4) [length (filter (== Just player) line) | line <- lineList]

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


main :: IO ()
main = do
    putStrLn "Welcome to Connect Four!"
    let board = initBoard
    gameLoop board True 12345  -- Start with player True and an initial seed

gameLoop :: Board -> Player -> Int -> IO ()
gameLoop board currentPlayer seed = do
    printBoard board
    if checkWin board (not currentPlayer) then
        putStrLn $ "Player " ++ show (not currentPlayer) ++ " wins!"
    else if all (isJust . (board !)) [(r, c) | r <- [0..5], c <- [0..6]] then
        putStrLn "It's a draw!"
    else do
        putStrLn $ "Player " ++ show currentPlayer ++ "'s turn."
        col <- getMove
        case makeMove board col currentPlayer of
            Just newBoard -> gameLoop newBoard (not currentPlayer) (xorshift seed)
            Nothing -> do
                putStrLn "Invalid move. Try again."
                gameLoop board currentPlayer seed

getMove :: IO Column
getMove = do
    putStr "Enter a column number (0-6): "
    hFlush stdout
    input <- getLine
    case reads input of
        [(col, "")] | col >= 0 && col <= 6 -> return col
        _ -> putStrLn "Invalid input. Please enter a number between 0 and 6." >> getMove

printBoard :: Board -> IO ()
printBoard board = mapM_ (putStrLn . show . map cellToChar) [[board ! (r, c) | c <- [0..6]] | r <- [0..5]]
  where
    cellToChar Nothing = '.'
    cellToChar (Just True) = 'X'
    cellToChar (Just False) = 'O'
