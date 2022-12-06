import Data.Functor.Product (Product(Pair))
data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)



-- 1.1

instance Show Expr where
  show :: Expr -> String
  show (Const i ) = show i
  show (e1 :+: e2) = "(" ++ show e1 ++ "+" ++ show e2
  show (e1 :*: e2) = "(" ++ show e1 ++ "+" ++ show e2

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x1 :+: x2) = (evalExp x1) + (evalExp x2)
evalExp (x1 :*: x2) = (evalExp x1) * (evalExp x2)

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

evalArb :: Tree -> Int
evalArb (Lf i) = i
evalArb (Node Mult t1 t2) = (evalArb t1) * (evalArb t2)
evalArb (Node Add t1 t2) = (evalArb t1) + (evalArb t2)






arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16


expToArb :: Expr -> Tree
expToArb (Const x) = (Lf x)
expToArb (x1 :+: x2) = (Node Add (expToArb x1) (expToArb x2))
expToArb (x1 :*: x2) = (Node Mult (expToArb x1) (expToArb x2))

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys c = [fst x | x<- toList c]
  values :: c key value -> [value]
  values c = [snd x | x <- toList c]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  fromList ((key, value):xs) = insert key value $ fromList xs


newtype PairList k v
  = PairList { getPairList :: [(k, v)] }


instance Collection PairList where
  empty = PairList []
  singleton a b = PairList [(a,b)]
  insert a b (PairList []) = PairList [(a,b)]
  -- insert a b (PairList ((k,v) : ls)) = PairList ((a, b) : (k,v) : ls)
  insert a b (PairList ls) = PairList $ filter(\(k,v) -> a /= k)  ls
  delete a (PairList ls) = PairList $ filter(\(k,v) -> k /=a ) ls
  clookup k (PairList l) = lookup k l 
  -- clookup k p = lookup k (getPairList p)
  toList = getPairList


data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty
  insert k v Empty = singleton k v 
  insert k v (BNode t1 k1 v1 t2)
    | k < k1 = BNode (insert k v t1) k1 v1 t2
    | k > k1 = BNode t1 k1 v1 (insert k v t2)
    | otherwise = BNode t1 k (Just v) t2
  clookup _ Empty = Nothing
  clookup k (BNode t1 k1 v1 t2)
    | k < k1 = clookup k t1
    | k > k1 = clookup k t2
    | otherwise = v1

  delete _ Empty = Empty
  delete k (BNode t1 k1 v1 t2)
    | k < k1 = BNode (delete k t1) k1 v1 t2
    | k > k1 = BNode t1 k1 v1 (delete k t2) 
    | otherwise = BNode t1 k1 Nothing t2
  
  toList Empty  = []
  toList (BNode t1 k1 v1 t2) = toList t1 ++ embed k1 v1 ++ toList t2
    where
      embed k (Just v) = [(k,v)]
      embed k Nothing = []

  toList (BNode t1 k1 (Just v1) t2) = [(k1,v1) | (k1,v1) <- toList t1] ++ [(k1,v1) | (k1,v1) <- toList t2]

  
