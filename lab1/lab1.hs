import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x

triple :: Integer -> Integer
triple x = x + x + x


--maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
               else y

max3 x y z = let
               u = maxim x y
             in 
               (maxim  u z)

max4 x y z t = let
                    u = max3 x y z
                in
                    (maxim u t)

testM x y z t = let u = max4 x y z t
                    in (u >= x && u >= y && u >= z && z >=z)


sumSq a b = a*a + b * b

isEven a = if (a `mod` 2 == 0)
      then "par"
      else "impar"

factorial :: (Eq a, Num a) => a -> a
factorial n = if n == 0 then 1 else n * factorial (n-1) 