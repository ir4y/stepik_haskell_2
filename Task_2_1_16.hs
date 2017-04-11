{-# LANGUAGE TypeOperators #-}
module Task_2_1_16 where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  foldMap f (Cmps cont) = foldMap (foldMap f) cont
