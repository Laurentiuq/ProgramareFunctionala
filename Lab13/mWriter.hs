import Distribution.Simple.Utils (xargs)

--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = do
  tell ("increment: " ++ show x ++"\n")
  return (x+1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
  y <- logIncrement x
  logIncrement y

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =  do
  if n == 1 then logIncrement x 
  else do
    y <- logIncrementN x (n-1)
    logIncrement y

newtype WriterLS a = Writer2 {runWriter2 :: (a, [String])}   
instance  Monad WriterLS where
  return va = Writer2 (va, [])
  ma >>= k = let (va, log1) = runWriter2 ma
                 (vb, log2) = runWriter2 (k va)
             in  Writer2 (vb, log1 ++ log2)



instance  Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma  



tellLS :: String -> WriterLS () 
tellLS log = Writer2 ((), [log])

logIncrementLS :: Int  -> WriterLS Int
logIncrementLS x = do
  tellLS ("increment: " ++ show x ++"\n")
  return (x+1)

logIncrementNLS :: Int -> Int -> WriterLS Int
logIncrementNLS x n =  do
  if n == 1 then logIncrementLS x 
  else do
    y <- logIncrementNLS x (n-1)
    logIncrementLS y