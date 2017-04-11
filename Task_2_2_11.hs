module Task_2_2_11 where

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f1 (Tr a b c) = Tr (f1 a) (f1 b) (f1 c)

instance Applicative Triple where
  pure a = Tr a a a
  Tr f1 f2 f3 <*> Tr a b c = Tr (f1 a) (f2 b) (f3 c)

instance Foldable Triple where
  foldr f init (Tr a b c) = foldr f init [a, b, c]

instance Traversable Triple where
  traverse f (Tr a b c) = Tr <$> (f a) <*> (f b) <*> (f c)
