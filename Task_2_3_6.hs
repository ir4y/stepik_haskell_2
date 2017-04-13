module Task_2_3_6 where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
  fmap f (Un a) = Un (f a)
  fmap f (Bi a b rest) = Bi (f a) (f b) (fmap f rest)

instance Applicative OddC where
  pure a = (Bi a a (pure a))
  (Bi fa _ _) <*> (Un a) = Un (fa a)
  (Un fa) <*> (Bi a _ _) = Un (fa a)
  (Bi fa fb frest) <*> (Bi a b rest) = Bi (fa a) (fb b) (frest <*> rest)

instance Foldable OddC where
  foldr f init (Un a) = f a init
  foldr f init (Bi a b rest) = (f a (f b (foldr f init rest)))


instance Traversable OddC where
  traverse f (Un a) = Un <$> (f a)
  traverse f (Bi a b rest ) = Bi <$> (f a) <*> (f b) <*> (traverse f rest)
