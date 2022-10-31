-- nrVocale :: [String] -> Int
-- nrVocale x =  
import Data.Char

nrvoc [] = 0
nrvoc(h:t) = if h =='a' || h=='e'|| h=='u' || h=='i'||h=='o'
            then 1 + nrvoc t
            else nrvoc t

nrVocale [] = 0
nrVocale(h:t) = if reverse h == h
                then nrvoc h + nrVocale t
                else nrVocale t




f _ [] = []
f x (h:ls) = if even h 
             then h : x : f x ls
             else h : f x ls


divizori x = [y | y<-[1..x], x `mod` y == 0]

listadiv [] = []
listadiv l = [divizori x | x <- 1]

inIntervalRec x y [] = []
inIntervalRec x y (l:ls) =
    if x <= l && l <= y
        then l : inIntervalRec x y ls
        else inIntervalRec x y ls

inIntervalComp x y l = [z | z <- l, z>= x && z<=y]


positiveRec :: (Ord a1, Num a2, Num a1) => [a1] -> a2
positiveRec [] = 0
positiveRec (x:ls) =
    if x > 0
        then 1 + positiveRec ls
        else positiveRec ls

positiveComp [] = 0
positiveComp l = length [z | z <- l, z >0]

pozitiiImpareComp ls = [i | (i,x) <- zip [0..] ls, odd x]
 
pozitiiImpareRec [] poz = []
pozitiiImpareRec (h:t) poz = 
    if odd h
        then poz : pozitiiImpareRec t (poz + 1)
        else pozitiiImpareRec t (poz + 1)

pozitiiImpareRecF ls = pozitiiImpareRec ls 0

pozitiiPareComp ls = [i | (i,x) <- zip [0..] ls, even x]




multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (h:t) =
    if isDigit h
        then digitToInt h  * multDigitsRec t
        else multDigitsRec t

multiDigitsRec ls = product [digitToInt z | z<-ls, isDigit z]