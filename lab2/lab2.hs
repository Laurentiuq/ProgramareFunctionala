fizzbuzz x
  | mod x 5 == 0 && mod x 3 == 0 = "FizzBuzz"
  | mod x 5 ==0 = "Buzz"
  | mod x 3 == 0 = "Fizz"


tribonacciCazuri :: (Eq t, Num t, Num a) => t -> a
tribonacciCazuri n
  | n == 1 = 1
  | n == 2 = 1
  | n == 3 = 2
  | otherwise = tribonacciCazuri(n - 1) + tribonacciCazuri(n-2) + tribonacciCazuri(n-3)

tribonacciEcuational 1 = 1
tribonacciEcuational 2 = 1
tribonacciEcuational 3 = 2
tribonacciEcuational n = tribonacciEcuational(n- 1) + tribonacciEcuational(n - 2) + tribonacciEcuational(n - 3)


tribonacciCazuri2 n
  | n <= 2 = 1
  | n == 3 = 2
  | otherwise = tribonacciCazuri2(n - 1) + tribonacciCazuri2(n-2) + tribonacciCazuri2(n-3)



bin :: (Eq t1, Eq t2, Num t1, Num a, Num t2) => t2 -> t1 -> a
bin n 0 = 1
bin 0 k = 1
bin n k = bin (n-1) k + bin (n-1) (k-1)


verifL x = not (mod (length x) 2 == 1 )

-- takefinal :: [Int] -> Int -> [Int]
-- takefinal ls n = drop (length ls - n) ls

takefinal :: [a] -> Int -> [a]
takefinal ls n = drop (length ls - n) ls


remove ls n = take n ls ++ drop (n + 1) ls


-- myreplicate n v 
--   | n==0 = []
--   | otherwise [v] ++ myreplicate (n-1) v

-- myreplicate 0 v = []
-- myreplicate n v = v:(myreplicate (n-1) v)

sumImp [] = 0
sumImp(h:t)
  | mod h 2/=0 = h + sumImp t
  | otherwise = sumImp t


totalLen [] = 0
totalLen (h:t)
  | length(h) > 0  && h !! 0 == 'A' = length h + totalLen t
  | otherwise = totalLen t




 
