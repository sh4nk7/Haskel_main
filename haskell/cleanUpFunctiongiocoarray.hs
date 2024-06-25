import System.Random
import Data.List
import Data.Array

type Matrice = Array (Int, Int) Bool  -- Tipo per rappresentare la matrice come un array
type Posizione = (Int, Int)  -- Tipo per rappresentare una posizione nella matrice

-- Funzione per invertire il valore di una cella e delle sue celle adiacenti
invertiCelle :: Matrice -> Int -> Int -> Matrice
invertiCelle matrice x y = 
    let range = ((x-1, y-1), (x+1, y+1))  -- Determina il range delle celle adiacenti alla cella corrente
        celleInvertite = [(i, j) | i <- [fst (fst range) .. fst (snd range)], j <- [snd (fst range) .. snd (snd range)], inRange (bounds matrice) (i, j)]  -- Lista delle celle da invertire
        matriceInvertita = matrice // [(cella, not (matrice ! cella)) | cella <- celleInvertite]  -- Applica l'inversione
    in matriceInvertita

-- Funzione per applicare l'operazione di pulizia
pulisci :: Matrice -> Posizione -> Matrice
pulisci matrice (x, y) = invertiCelle matrice x y

-- Funzione per generare una matrice pulita di dimensioni specificate
generaMatricePulita :: Int -> Int -> IO Matrice
generaMatricePulita w h = do
    gen <- newStdGen
    let limitiMatrice = ((0, 0), (h-1, w-1))  -- Limiti della matrice
        valoriCasuali = take (w * h) $ randomRs (False, True) gen  -- Lista di valori casuali
        matriceLista = assocs $ listArray limitiMatrice valoriCasuali  -- Lista degli elementi della matrice
    return $ array limitiMatrice matriceLista  -- Costruzione della matrice

-- Funzione per generare una lista di posizioni casuali all'interno della matrice
generaPosizioniCasuali :: Int -> Int -> Int -> IO [Posizione]
generaPosizioniCasuali w h m = do
    gen <- newStdGen
    let posizioniCasuali = take m $ nub $ zip (randomRs (0, h - 1) gen) (randomRs (0, w - 1) gen)  -- Lista di posizioni casuali
    return posizioniCasuali

-- Funzione per giocare il gioco di pulizia
gioca :: Int -> Int -> Int -> Matrice -> IO ()
gioca w h m matrice = do
    -- Genera posizioni casuali
    posizioniCasuali <- generaPosizioniCasuali w h m
    -- Applica mosse casuali
    let matriceDopoMosseCasuali = foldl (\m p -> pulisci m p) matrice posizioniCasuali
    -- Stampa la matrice dopo le mosse casuali
    putStrLn "Matrice Iniziale dopo le Mosse Casuali:"
    stampaMatrice matriceDopoMosseCasuali
    putStrLn ""
    -- Inizia il turno dell'utente
    mosseUtente <- turnoUtente matriceDopoMosseCasuali 0
    putStrLn $ "Congratulazioni! Hai risolto il puzzle in " ++ show mosseUtente ++ " mosse!"

-- Funzione per il turno dell'utente
turnoUtente :: Matrice -> Int -> IO Int
turnoUtente matrice mosse = do
    putStrLn "Inserisci 'x y' per pulire la cella alla posizione (x, y), o 'q' per uscire:"
    input <- getLine
    if input == "q"
        then return mosse
        else do
            let inputList = words input
            if length inputList /= 2
                then do
                    putStrLn "Input non valido. Inserisci due numeri interi separati da spazio."
                    turnoUtente matrice mosse
                else do
                    let [x, y] = map read inputList
                    if x < 0 || x >= h matrice || y < 0 || y >= w matrice
                        then do
                            putStrLn "Input non valido. Coordinate fuori dai limiti della matrice."
                            turnoUtente matrice mosse
                        else if not $ matrice ! (x, y)
                            then do
                                putStrLn "Cella già pulita. Scegli un'altra cella."
                                turnoUtente matrice mosse
                            else do
                                let matriceAggiornata = pulisci matrice (x, y)
                                putStrLn "Matrice dopo il tuo movimento:"
                                stampaMatrice matriceAggiornata
                                if all (== False) (elems matriceAggiornata)
                                    then return (mosse + 1)
                                    else turnoUtente matriceAggiornata (mosse + 1)

-- Funzione per stampare la matrice
stampaMatrice :: Matrice -> IO ()
stampaMatrice matrice = 
    let ((r1, c1), (r2, c2)) = bounds matrice  -- Limiti della matrice
        stampaRiga r = putStrLn $ unwords [if matrice ! (r, c) then "1" else "0" | c <- [c1..c2]]  -- Funzione per stampare una riga
    in mapM_ stampaRiga [r1..r2]

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
    matricePulita <- generaMatricePulita w h
    gioca w h m matricePulita
