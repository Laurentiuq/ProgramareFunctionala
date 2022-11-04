-- Ex1
sumImpare :: [Int] -> Int
sumImpare ls = foldl (+) 0 . map (^2) . filter (odd) $ ls

--Ex2
verifica :: [Bool] -> Bool
verifica = foldr (&&) True

--Ex3

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies prop ls = foldl (&&) True (map prop ls)

--Ex4
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies prop ls = foldl (||) False (map prop ls)

--Ex5

--v1

-- mapFoldr f = foldr((:).f)[]
-- filterFoldr pred = foldr(\a -> if pred a then (a:) else (id)) []
-- filterFoldr pred = (\a l -> if pred a then a:l else l)

--v2

mapFoldr f = foldr (\x xs -> f x : xs) []
filterFoldr f = foldr (\x xs -> if f x then x : xs else xs) []

-- listToInt :: [Integer] -> Integer
listToInt ls = foldl (\x xs-> x * 10 + xs) 0  ls


--7

-- rmChar c str = foldl (\str (x:xs) -> if x == c then (xs) else (x:xs)) str
--(a)


rmChar a s = filter (/= a) s
-- rmChar char string = filter(/= char) string

--(b)

rmCharsRec :: String -> String -> String
rmCharsRec [] b = b
-- rmCharsRec a [] = []
rmCharsRec (x:xa) b = 
    {-
     if elem x b
     then rmCharsRec xa (rmChar x b)
     else rmCharsRec xa b 
    -}
    rmCharsRec xa (rmChar x b)
 
rmCharsRec2Rec [] = id
rmCharsRec2(c:s) = rmCharsRec2 s . rmChar c

 -- (c)
rmCharsFold :: String -> String -> String
rmCharsFold a b = foldr rmChar b a

