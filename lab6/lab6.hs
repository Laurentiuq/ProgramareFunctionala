{-# LANGUAGE LambdaCase #-}
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]


 -- a) Scrieti o functie care indica daca un fruct e o portocala de Sicilia sau nu. Soiurile
 -- de portocale din Sicilia sunt Tarocco, Moro si Sanguinello


{-
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala soi felii)
    | soi == "Tarocco" || soi == "Moro" || soi == "Sanguinello" = True
    | otherwise = False
-}

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _ ) = False
ePortocalaDeSicilia (Portocala soi _) = soi == "Tarocco" || soi == "Moro" || soi == "Sanguinello" 


test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False


-- b) Scrieti o functie care calculeaza numarul total de felii ale portocalelor de Sicilia dintr-o lista de fructe.

{-
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia ls = foldl (+) 0 (map(\ (Portocala soi nr) -> nr) (filter ePortocalaDeSicilia ls))
-}

{-
nrFeliiPortocala list = sum [felii| Portocala soi felii <- list, ePortocalaSicilia(Portocala soi felii)]
-}

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = sum . map(\ (Portocala soi nr) -> nr) . filter ePortocalaDeSicilia 

test_nrFeliiSicilia :: Bool
test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52


-- c) Scrieti o functie care calculeaza numarul de mere care au viermi dintr-o lista de fructe

{-
eMarCuViermi :: Fruct -> Int
eMarCuViermi (Portocala _ _) = 0
eMarCuViermi (Mar soi vierme) = if vierme then 1 else 0

nrMereViermi :: [Fruct] -> Int
nrMereViermi = sum . map eMarCuViermi
-}


nrMereViermi l = length [1 | Mar _ areViermi <- l, areViermi]

-- nrMereViermi l = length [1 | Mar _ True <- l]


-- nrMereViermi = sum . map(\f -> case f of
                        --    (Mar _ True) -> 1
                        --    _ -> 0)

test_nrMereViermi = nrMereViermi listaFructe == 2




--Exercitiul 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

-- a) Scrieti o functie care intoarce "Meow!" pentru pisica si "Wofof!" pentru caine
{-
vorbeste :: Animal -> String
vorbeste a = case a of
                (Pisica _) -> "Meow!"
                (Caine _ _) -> "Woof!"
-}
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"


-- b) Scrieti o functie care intoarce rasa unui caine dat ca parametru sau Nothing daca
-- parametrul este o pisica (Maybe)
rasa :: Animal -> Maybe String
rasa (Pisica _)  =  Nothing
rasa (Caine _ rasa) = Just rasa



--Exercitiul 3

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show
-- a) Scrieti o functie care verifica daca suma elementelor de pe fiecare linie este egala
-- cu o valoare n. Rezolvati cerinta folosind foldr.
{-
verifica :: Matrice -> Int -> Bool
verifica (M matr) n = foldr (||) False (map (\ (L linie) -> n == sum linie) matr)
-}

verifica :: Matrice -> Int -> Bool
verifica (M matr) n = foldr ((||) . (\ (L linie) -> n == sum linie)) False matr

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True


-- b) Scrieti o functie doarPozN care are ca parametru un element de tip Matrice si un
-- numar intreg n, si care verifica daca toate liniile de lungime n din matrice au
-- numai elemente strict pozitive.


{-
doarPozN :: Matrice -> Int -> Bool
doarPozN (M matr) n = and (map (\(L l) -> foldl (\rez x -> rez && x >= 0) True l) (filter (\(L l) -> length l == n ) matr))
-}

{-

    -- rezolvare lab
    verif (L list) n = if length list == n
                        then length(filter(>0) list) == n
                        else True

    doarPozN(M m) n = foldr(\(L list) x -> x && verif (L list) n) True m

-}

doarPozN :: Matrice -> Int -> Bool
doarPozN (M matr) n = and . map (\(L l) -> foldl (\rez x -> rez && x >= 0) True l) . filter (\(L l) -> length l == n ) $ matr


testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False




-- c) Definiti predicatul corect care verifica daca toate liniile dintr-o matrice au aceeasi lungime


{-
    -- rezolvare lab

    corect(M (x:y:xs)) = 
        length first  == length second && corect (M(y:xs))
        where
            L first = x
            L second = y
    corect(M _ ) = True

-}

corect :: Matrice -> Bool
corect (M matr) = all (==aux) lungimi
                where
                    lungimi = map (\(L l) -> length l) matr
                    aux = head lungimi
                

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True
