{-# LANGUAGE TypeOperators #-}
module Task_2_2_15 where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f|.|g) where
  fmap fun (Cmps x) = Cmps (fmap (fmap fun) x)

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  Cmps fa <*> Cmps a = Cmps $ fmap (<*>) fa <*> a

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  foldMap f (Cmps cont) = foldMap (foldMap f) cont

instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
  traverse f (Cmps x) = Cmps <$> (traverse (traverse f) x)
