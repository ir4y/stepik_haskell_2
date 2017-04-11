module Task_2_2_14 where

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l x r) = (Branch (fmap f l) (f x) (fmap f r))

instance Applicative Tree where
  pure a = (Branch Nil a Nil)
  (Branch fl fa fr) <*> (Branch l a r) = (Branch (fl <*> l) (fa a) (fr <*> r))
  Nil <*> _ = Nil
  _ <*> Nil = Nil

instance Foldable Tree where
  foldr f init Nil = init
  foldr f init (Branch l x r) = (foldr f (f x (foldr f init r)) l)

instance Traversable Tree where
  traverse f Nil = pure Nil
  traverse f (Branch l x r) = Branch <$> (traverse f l) <*> (f x) <*> (traverse f r)
