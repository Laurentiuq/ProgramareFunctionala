--1
factori::Int -> [Int]
factori x = [d | d<-[1..x `div` 2], mod x d == 0] ++ [x]

--2
prim :: Int -> Bool
prim x = factori x == [1,x]

--3
numerePrime :: Int -> [Int]
numerePrime n = [x | x<-[2..n], prim x]

--4
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 x y z = [(a,b,c)| ((a,b) ,c) <- zip (zip x y) z]


--5

firstEl :: [(a,b)] -> [a]
firstEl  = map fst

--6

sumList :: [[Int]]->[Int]
sumList = map sum

--7

pre12 :: [Int] -> [Int]
pre12 [] = []
pre12 (x:xs)
    | odd x = (x * 2) : pre12 xs
    | otherwise = (div x 2) : pre12 xs


--7 v2
       
prel2 = map(\i -> if even i then (div i 2)
                          else (i * 2))


--8

f8 :: Char -> [String] ->[String]
f8 x ls = filter (\a -> elem x a) ls

--9

modder l = map(^2)(filter odd l)

modder2  = map(^2) . filter odd


--10


patrate ls = map(\(a,b) -> b^2).filter(odd.fst).zip[1..]

--11

isVowel c = c `elem` "aeiouAEIOU"

numaiVocale :: [String] -> [String]
numaiVocale = map (filter isVowel)

--12



mymap :: (a->b)->[a]->[b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs



myfilter :: (a->Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs)
        | f x = x : (myfilter f xs)
        | otherwise = myfilter f xs