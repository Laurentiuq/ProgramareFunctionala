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