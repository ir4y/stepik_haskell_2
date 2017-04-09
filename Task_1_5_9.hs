{-# LANGUAGE TypeOperators #-}
module Task_1_5_9 where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a)} deriving Show

instance (Functor f, Functor g) => Functor (f|.|g) where
  fmap fun (Cmps x) = Cmps (fmap (fmap fun) x)

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap (fmap getCmps) . unCmps3
