module Task_2_2_12 where

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
  fmap f (Ok a) = Ok (f a)
  fmap f (Error text) = Error text

instance Applicative Result where
  pure a = Ok a
  (Ok f) <*> (Ok v) = Ok (f v)
  (Error text) <*> _ = Error text
  _ <*> (Error text) = Error text

instance Foldable Result where
  foldr f init (Ok a) = f a init

instance Traversable Result where
  traverse f (Ok a) = Ok <$> (f a)
  traverse _ (Error text) = pure (Error text)
