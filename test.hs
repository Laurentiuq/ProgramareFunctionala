import Data.Char
 -- Dupa ordinea in care apar in curs, NU dupa denumire


--Curs 1

--Quizz 1
-- Cum se comenteaza o linie in Haskell?
--

-- x = let x=3 in x *5
-- 15

--x = let x = 3, y= 4 in x *y
--eroare, trebuie separate prin ;


--Quizz2

-- Ce tip are o functie foo care are doua argumente, primul argument de tip Char, iar al doilea argument de tip Bool, si intoarce un rezultat de tip Bool?
-- foo::Char->Bool->Bool

-- :t [True, 'a', "FP"]
-- eroare

-- :t (True, 'a', "FP")
-- (True, 'a', "FP") :: (Bool, Char, String)







-- Curs 2

--Quizz 1

-- Ce tip are o functie foo care are doua argumente, o functie de la Char la Bool si, respectiv, un Char, si intoarce un Bool?
-- foo::(Char->Bool)->Char->Bool

-- Ce valoare are f 3 in f 5 = let x = 3 in x + x?
-- exceptie ( nu se potriveste niciun caz din definitia lui f)

-- Ce valoare are f 5 in f x = let x = 3 ; y = 4 in x + y?
-- 7


--Quizz 2

-- Cum putem defini lista [3,4,5,6]?
-- 3:4:5:6:[]
-- [3..6]
-- 3:4:5:[6]

-- Ce obtinem dupa instructiunile?
-- Prelude> let xs = [1,2,3]
-- Prelude> let ys = [11,12]
-- Prelude> zip xs ys
-- [(1,11),(2,12)]

-- Ce obtinem dupa instructiunile?
-- Prelude> let natural = [0..]
-- Prelude> natural !! 5
-- 5



-- Curs 3

-- Quizz 1

-- f x = x + x
-- g x = x * x
-- g.f $ 3
-- 36

-- ([1,2,3] ++) [4,5,6]
-- [1,2,3,4,5,6]

-- reverse.take 3 [1..10]
-- eroare:
-- ghci> :t reverse
-- reverse :: [a] -> [a]
-- ghci> :t take
-- take :: Int -> [a] -> [a]
-- (reverse.(take 3)) [1..10] -> merge
-- reverse.(take 3) $ [1..10] -> merge
-- reverse.take 3 $ [1..10] -> merge



--Quizz 2

-- map (+1) [1,2,3,4]
-- [2,3,4,5]

-- map (1-) [1,2,3,4]
-- [0,-1,-2,-3]
-- map (-1) [1,2,3,4] -> Eroare
-- map (-1+) [1,2,3,4] -> Merge

-- map toUpper "abcd"
-- "ABCD"


-- Quizz 3

-- length.filter (=='a') $ "abracadabra"
-- instructiune invalida

-- length.filter (=='a') $ "abracadabra"
-- 5

-- filter (\x -> (rem x 2) == 0) [1..10]
-- [2,4,6,8,10]







-- Curs 4

-- Quizz 1

foo1::(Int,Char, String) -> String
foo1 (x, _, _) = "salut";
-- let  cf = curry foo1;
-- curry foo1 - nu se poate curry pe functie cu 3 argumente

foo2::(Int, (Char, String)) ->String
foo2 (x, (_ , _)) = "salut";
-- curry foo2 :: Int -> (Char, String) -> String

foo3 :: Int -> Char -> String
foo3 x  _  = "Salut";
-- uncurry foo3 :: (Int, Char) -> String


-- Quizz2


-- foldr (++) ["woot", "WOOT", "woot"]
-- instructiune invalida -> lipseste elementul neutru

-- foldr (&&) True [False, True]
-- False

-- foldr (\ x y -> concat ["(", x, "+", y, ")"]) "0" ["1","2", "3", "4", "5"]
-- "(1+(2+(3+(4+(5+0)))))"



--Quizz3

-- foldl (^) 2 [1..3]
-- 64


-- foldr (^) 2 [1..3]
-- 1

-- foldr (:) [] [1..3]
-- [1,2,3]

-- foldl (flip (:)) [] [1..3]
-- [3,2,1]



--Curs 5


-- Fie tipul de date:
data Doggies a =
       Husky a
    | Mastiff a


--Ce este doggies:
-- constructor de tip

--Ce tip are Mastiff "Scooby Doo"
-- Doggies [Char]

--Ce tip are Husky (10 :: Integer)
--Doggies Integer



-- Curs 6

--1. Clasa Eq
-- face testarea egalitatii posibila

--2. Sa presupunem ca clasa de tipuri Ord are operatorul >. Ce tip are >?
-- Ord a => a -> a -> Bool

--3. Ce puteti sa spuneti despre codul de mai jos?

{-
data Mood = Blah
            | Woot
            deriving Show

settleDown x = if x == Woot
                    then Blah
                    else x

-}

-- codul nu este corect pentru ca nu exista o instanta a clasei Eq pentru tipul Mood

data Mood = Blah
            | Woot
            deriving (Show)

-- instance Eq Mood where
--         (==) Blah Blah = True
--         Woot == Woot = True
                
instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    
settleDown x = if x == Woot
                    then Blah
                    else x


f :: (Int, (Char, Char, Char)) -> Int
f (1, ('a', 'b', 'a')) = 1

swap ::(a, b) -> (b, a)
swap (x, y) = (y, x)

first2 (x:y:xs) = (x, y)

-- safetail xs | null xs = []
            -- | otherwise = tail xs

safetail [] = []
safetail x = tail x

-- False || False = False
-- _ || True = True




--ceva :: (Ord a, Num a) => [a] -> [a]
-- ceva xs = [x| x<-xs, x > 0, x >= 7]
--
--pyths n = [(x,y,z)| x<-[1..n], y<-[1..n], z<-[1..n], x*x + y*y == z*z]


-- data Coord a  = X a | Y a


-- instance Eq a => Eq (Coord a) where
    -- (==) (X x1) (X x2) = x1 == x2
    -- (==) (Y y1) (Y y2) = y1 == y2


-- x = X 5
-- y = X 5
-- foo x = x==x


f1 [] = []
f1 [x] = [x]
f1 l1 = let x:y:z = l1 in x + y : f1 z

f2 :: String -> Int
f2 st = length st
f3 :: Int -> String
f3 n = "ab"

f4 :: (String -> Int) -> (Int -> String) -> Int -> Int
f4 x y t = t 


f5 :: (Int, String, String) -> Int 
f5 (_, _, _) = 1

f6 [] = []
f6 [x] = [x]
f6 l1 = let x:y:z = l1 in x + y:f6 z


f7 :: (Int, (Int, Int))-> Bool -> Int
f7 (_, (_, _)) _ = 1

-- :t uncurry f7
-- uncurry f7 :: ((Int, (Int, Int)), Bool) -> Int

f8 :: (Int, Int) -> (Int, Int) -> Int -> Bool
f8 (_, _) (_, _) _ = True

-- :t curry f8
-- curry f8 :: Int -> Int -> (Int, Int) -> Int -> Bool

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id


data Point a b = Pt a b

data Maybes a = Nothings | Justs a

ps :: Maybes Int  -> Bool
ps Nothings = False
ps (Justs 7) = True
ps (Justs _) = False

data Eithers a b = Lefts a | Rights b
mylist :: [Eithers Int String]
mylist = [Lefts 4, Rights "Hello", Lefts 5, Rights " world!"]


ff :: Eithers Int String -> String
ff (Rights a) = a
ff (Lefts _) = ""

fE :: [Eithers Int String] -> String
fE ls = foldl (++) "" . map(\x -> case x of
                                    (Rights y) -> y
                                    (Lefts _) -> "") $ ls

fE2 :: [Eithers Int String] -> [Char]
fE2 = concatMap ff

lft :: Eithers Int String -> String
lft (Lefts a) = "Nu e string"
lft (Rights a) = a


data Nus = Ni | Na
nustiu :: Nus -> String
-- nustiu (Ni a) = "Ni"
nustiu Na = "Na"

instance Show Nus where
    show :: Nus -> String
    show Ni = "Ni"
    show Na = "Na"