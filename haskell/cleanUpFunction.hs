type Matrice = [[Bool]]

-- Funzione per invertire il valore di una cella e delle sue celle adiacenti
invertiCelle :: Matrice -> Int -> Int -> Matrice
invertiCelle matrice x y = 
    let invertiCella (r, riga) = 
            if r == x 
                then let rigaInvertita = map (\(c, val) -> if abs (c - y) <= 1 && abs (r - x) <= 1 && (c /= y || r /= x) then not val else val) (zip [0..] riga) 
                     in rigaInvertita
                else riga
    in map invertiCella (zip [0..] matrice)

-- Funzione per applicare l'operazione di pulizia
cleanUpFunction :: (Int, Int) -> [Bool] -> Int -> [Bool]
cleanUpFunction (colonne, righe) matrice (x, y) =
    let matrice2D = suddividiInBlocchi colonne matrice  -- Converti la lista in una matrice 2D
        matriceAggiornata2D = invertiCelle matrice2D x y  -- Applica l'operazione di pulizia
    in concat matriceAggiornata2D  -- Converti la matrice 2D in una lista

-- Funzione per suddividere una lista in blocchi di una data dimensione
suddividiInBlocchi :: Int -> [a] -> [[a]]
suddividiInBlocchi _ [] = []
suddividiInBlocchi n xs = take n xs : suddividiInBlocchi n (drop n xs)
