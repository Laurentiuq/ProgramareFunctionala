data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a


instance Show Punct where
  show :: Punct -> String
  show (Pt []) = "()"
  show (Pt (x:xs)) = "(" ++ showSec x xs ++ ")"
        where
            showSec x [] = show x
            showSec x (y:ys) = show x ++ ", " ++ showSec y ys


instance ToFromArb Punct where
  toArb (Pt x) = Vid
  toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

  fromArb(Vid) = Pt []
  fromArb(F x) = Pt[x]
  fromArb(N a b) = fromArb a *++* fromArb b
    where Pt l1 *++* Pt l2 = Pt(l1++l2)
  -- fromArb(N a b) = Pt(l ++ r)
        -- where
          -- Pt l = fromArb a
          -- Pt r = fromArb b
    
    
-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a


instance GeoOps Geo where
  perimeter :: Floating a => Geo a -> a
  perimeter (Square x) = 4 * x;
  perimeter (Rectangle l1 l2) =  2 * (l1 + l2) 
  perimeter (Circle x) = 2 * pi * x
  area (Square x) = (^2) x
  area (Rectangle l1 l2) = l1 * l2
  area (Circle x) = pi * ((^2)  x)

instance (Floating a, Eq a) => Eq (Geo a)  where
  (==) :: Geo a -> Geo a -> Bool
  (==) f1 f2 = perimeter f1 == perimeter f2

  -- (==) (Square x1) (Square x2) = perimeter (Square x1) == perimeter (Square x2)
  -- (==) (Rectangle l1 l2) (Rectangle l3 l4)  = perimeter (Rectangle l1 l2) == perimeter (Rectangle l3 l4)
  -- (==) (Circle x1) (Circle x2) = perimeter (Circle x2) == perimeter (Circle x2)

-- ghci> pi
-- 3.141592653589793