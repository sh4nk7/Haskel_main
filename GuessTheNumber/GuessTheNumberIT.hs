{-
Indovina il numero
Estrai un numero segreto, una sola volta, tra 1 e 90
L'utente deve indovinarlo, ripetutamente
Per ogni tentativo, fornisci un suggerimento all'utente
Comunica se il tentativo è inferiore o superiore al segreto
In aggiunta all'esercizio
Conta e comunica il numero di tentativi
Consenti non più di 10 tentativi
-}



import System.Random (randomRIO)
import Control.Monad (forM_)

main :: IO ()
main = do
    -- Genera un numero casuale tra 1 e 90
    secretNumber <- randomRIO (1, 90)
    putStrLn "Indovina il numero segreto tra 1 e 90. Hai 10 tentativi!"

    -- Cicla fino a un massimo di 10 tentativi
    forM_ [1..10] $ \attempt -> do
        putStrLn $ "Tentativo " ++ show attempt ++ " di 10. Inserisci il tuo tentativo:"
        guess <- getLine
        let guessNumber = read guess :: Int
        if guessNumber == secretNumber then do
            putStrLn "Complimenti! Hai indovinato il numero segreto!"
            putStrLn $ "Ci hai messo " ++ show attempt ++ " tentativi."
            return ()
        else do
            let hint = if guessNumber < secretNumber then "Troppo basso!" else "Troppo alto!"
            putStrLn hint
            if attempt == 10 then do
                putStrLn "Hai esaurito i tentativi!"
                putStrLn $ "Il numero segreto era: " ++ show secretNumber
                return ()
            else
                return ()

    -- Se si utilizzano tutti i tentativi, il programma terminerà automaticamente.
