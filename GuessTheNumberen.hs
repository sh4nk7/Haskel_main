{-
Guess the number
Extract a secret number, just once, between 1 and 90
The user has to guess it, repeatedly
For any try, provide a suggestion to the user
Tell if the guess is below or above the secret
In addition to the exercise
Count and tell the number of tries
Allow no more than 10 tries
-}

import System.Random (randomRIO)
import Control.Monad (forM_)

main :: IO ()
main = do
    -- Generate a random number between 1 and 90
    secretNumber <- randomRIO (1, 90)
    putStrLn "Guess the secret number between 1 and 90. You have 10 tries!"

    -- Loop through up to 10 attempts
    forM_ [1..10] $ \attempt -> do
        putStrLn $ "Attempt " ++ show attempt ++ " of 10. Enter your guess:"
        guess <- getLine
        let guessNumber = read guess :: Int
        if guessNumber == secretNumber then do
            putStrLn "Congratulations! You guessed the secret number!"
            putStrLn $ "It took you " ++ show attempt ++ " tries."
            return ()
        else do
            let hint = if guessNumber < secretNumber then "Too low!" else "Too high!"
            putStrLn hint
            if attempt == 10 then do
                putStrLn "You've run out of tries!"
                putStrLn $ "The secret number was: " ++ show secretNumber
                return ()
            else
                return ()

    -- If all attempts are used, the program will automatically end.
