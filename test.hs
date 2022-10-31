import Data.Char
 -- Dupa ordinea in care apar in curs, NU dupa denumire
 
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