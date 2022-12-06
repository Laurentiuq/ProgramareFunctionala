import Text.Read (Lexeme(Ident))
{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
newtype Identity a = Identity a

instance Functor Identity where 
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)

data Constant a b = Constant b

instance Functor (Constant a) where
    fmap :: (b -> c) -> Constant a b -> Constant a c
    fmap f (Constant y) = Constant (f y)

data Two a b = Two a b

instance Functor (Two a) where
    fmap :: (b->c) -> Two a b -> Two a c
    fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three a b c)  = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap :: (b -> c) -> Three' a b -> Three' a c
    fmap f (Three' x y z) = Three' x (f y) (f z) 

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap :: (d -> e) -> Four a b c d -> Four a b c e
    fmap f (Four x y z t) = Four x y z (f t)

data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where
    fmap :: (b -> c) -> Four'' a b -> Four'' a c
    fmap f (Four'' x y z t) = Four'' x y z (f t)

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap :: (b -> c) -> Quant a b -> Quant a c
    fmap f Finance =  Finance
    fmap f (Desk x) = Desk x
    fmap f (Bloor b) = Bloor (f b)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    -- fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
    fmap fc (LiftItOut x) = LiftItOut (fmap fc x)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap fct (DaWrappa f g) = DaWrappa (fmap fct f) (fmap fct g)  

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap fct (IgnoringSomething f g) = IgnoringSomething f (fmap fct g)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap fct (Notorious x y z) = Notorious x y (fmap fct z)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z)  = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print x a) = Print x (f a)
    fmap f (Read g) = Read (fmap f g)
