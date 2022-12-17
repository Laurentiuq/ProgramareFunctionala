-- import Prelude hiding (concat)
-- import Prelude
{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)


concat1 :: List a -> List a -> List a
concat1 (Cons a ls) ld = Cons a (concat1 ls ld)
concat1 Nil ls = Nil

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b) 

instance Applicative List where
    pure a = Cons a Nil
    -- Cons f lf <*> ls = concat1 (fmap f ls) (fmap lf <*> ls)
    Nil <*> _ = Nil

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

-- noNegative :: Int -> Maybe Int
-- noNegative x = if x < 0 then Nothing else Just x

noNegative :: Int -> Maybe Int
noNegative x 
             | x < 0 = Nothing
             | otherwise = Just x

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

-- cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString name age weight
                    -- | noEmpty name == Nothing || noNegative age == Nothing || noNegative weight == Nothing = Nothing
                    -- | otherwise = Just Cow{name, age, weight}

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight = fmap Cow (noEmpty name) <*> noNegative age <*> noNegative weight
test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength a s
             | length s < a = Just s
             | otherwise = Nothing

test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName s = fmap Name (validateLength 25 s)
-- mkName s 
        -- | validateLength 25 s == Nothing = Nothing
        -- | otherwise = Just (Name s)


mkAddress :: String -> Maybe Address
mkAddress s = fmap Address (validateLength 100 s)
-- mkAddress s
        -- | validateLength 100 s == Nothing = Nothing
        -- | otherwise = Just (Address s)
test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson name address = fmap Person (mkName name) <*> mkAddress address
-- mkPerson name address 
                -- | mkName name == Nothing || mkAddress name == Nothing = Nothing
                -- | otherwise = Just (Person (Name name) (Address address))
test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))
