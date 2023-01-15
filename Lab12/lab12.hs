elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 nr ls = foldr (\a b -> if nr == a then True || b else b ) False ls 

newtype All = All {getAll :: Bool}
            deriving Show

instance Semigroup All where
    All a <> All b = All (a || b)

instance Monoid All where
    mempty = All False

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 nr ls = getAll $ foldMap (\a -> All (nr == a)) ls

null1 :: (Foldable  t) => t a -> Bool
null1 = foldr (\_ _ -> False) True 

null2 :: (Foldable  t) => t a -> Bool
null2 ls = not $ getAll $ foldMap (\a -> All True) ls

length1 :: (Foldable t) => t a -> Int
-- length1 = undefined
length1 = foldr (\_ b -> 1 + b ) 0 

newtype Add = Add {getAdd :: Int}
            deriving Show

instance Semigroup Add where
    Add a <> Add b = Add (a + b) 

instance Monoid Add where
    mempty = Add 0

length2 :: (Foldable t) => t a -> Int
length2 ls = getAdd $ foldMap (\a -> Add 1) ls

toList1 :: (Foldable t) => t a -> [a]
toList1  = foldr (:) [] 

newtype Concat a = Concat {getConcat :: a}

instance Semigroup (Concat [a])  where
    Concat a <> Concat b = Concat (a ++ b)

instance Monoid (Concat [a]) where
    mempty = Concat []

toList2 :: (Foldable t) => t a -> [a]
toList2 ls = getConcat $ foldMap (\a -> Concat [a]) ls

toList3 ls = foldMap (\a -> [a]) ls

fold1 :: (Foldable t, Monoid m) => t m -> m
-- fold1 = foldMap (<> mempty)
fold1 = foldMap id

data Constant a b = Constant b

instance Foldable (Constant a) where
    -- foldMap :: Monoid m => (a -> m) -> Constant-> m
    foldMap f (Constant b) =  f b

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap :: Monoid m => (a2 -> m) -> Two a1 a2 -> m
    foldMap f (Two a b) = f b 

data Three a b c =  Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b    

instance Foldable (Three' a) where
    foldMap f (Three' a b c) = f b <> f c

data Four a b = Four a b b b

instance Foldable (Four a) where
    foldMap :: Monoid m => (a2 -> m) -> Four a1 a2 -> m
    foldMap f (Four a b c d) = f b <> f c <> f d 

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable GoatLord  where
    foldMap m NoGoat = mempty
    foldMap m (OneGoat a) = m a
    foldMap m (MoreGoats a b c) = foldMap m a <> foldMap m b <> foldMap m c


elem3 nr = foldr cmp False
        where cmp x y 
                | nr == x = True
                | otherwise = y

newtype NewType a = Type a