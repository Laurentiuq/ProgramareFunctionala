-- {-# LANGUAGE UndecidableInstances #-}
import Data.Char
data Mood x = Blah x
            | Woot


instance (Eq x) => Eq (Mood x) where
    -- (Blah x) == (Blah y) = x == y
    (==) :: Eq x => Mood x -> Mood x -> Bool
    Woot == Woot = True
    (==) (Blah x) (Blah y) = x == y
    (==) (Blah x) Woot = False
    (==) Woot (Blah x) = False

instance (Show x) => Show (Mood x) where
    show :: (Show x) => Mood x -> String
    show Woot = "Woot"
    show (Blah x) = show x


-- settleDown "" (Mood x) -> (Mood x)
settleDown :: (Eq x, Num x) => Mood x -> Mood x
settleDown x = if x == Blah 7
                then Woot
                else x



data Ss = T
    deriving (Show, Eq)

data Reteta = Li [Ingredient]
    deriving Show
 
data Ingredient = Ing String Int Ss
    deriving Show
 
instance Eq Ingredient where
    (Ing nume1 cantitate1 T) == (Ing nume2 cantitate2 T) = map toUpper nume1 == map toUpper nume2 && cantitate1 == cantitate2 && T == T
 
instance Eq Reteta where
    (Li ls1) == (Li ls2) = ls1 == ls2 || (contains ls1 ls2 && contains ls2 ls1)
 
contains :: (Eq a) => [a] -> [a] -> Bool
contains [] [] = True
contains [] _ = True
contains _ [] = True
contains (x : xs) ys = elem x ys && contains xs ys
 
r4 = Li [Ing "lapte" 200 T, Ing "CEreale" 100 T]
r5 = Li [Ing "cereALE" 100 T, Ing "LAPTE" 200 T]
r6 = Li [Ing "cereALE" 100 T, Ing "Lapte" 300 T]

data Point a b = Pt a b

instance (Eq a, Eq b) => Eq (Point a b) where
    (==) (Pt x1 y1) (Pt x2 y2) = (x1 == x2) && (y1 == y2)


data PointInt = PtInt Int Int

instance  Eq PointInt where
    (==) (PtInt x1 y1) (PtInt x2 y2) = x1 == x2



h x = x ++ f x
    where f x = "a"

-- l1  = [1,5..]
-- l2 = [2,6..]

-- l3 = take 3 $ zip l1 l2

-- f xs = foldr (||) True [x `mod` 3 > 0 | x <- xs]

-- f1::Int -> Char -> Char

-- f1 x y = 'a'

-- l1 = [2,4..]
-- l2 = [10,20..]
-- l3 = zip l1 l2

data Price = Price Integer


l1 = ['a'..]
l2 = [1..3]
-- l3 = zip l1 . (zip l1 l2)

-- p = take 3 $ foldl (-) 0 [100,99..]


data Exp a = Val a | (Exp a) :*: (Exp a) | (Exp a) :+: (Exp a)
-- data Exp Int = Intm| (Exp a) :*: (Exp a) | (Exp a) :+: (Exp a)

