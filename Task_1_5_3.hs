{-# LANGUAGE TypeOperators #-}
module Task_1_5_3 where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a)}

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

type D t = (Maybe |.| []) t

a :: A
a = Cmps (1, ('a', True))

b :: B t
b = Cmps (True, id, Right 1)

c :: C
c  = Cmps (\b -> \a -> a)

d :: D Integer
d = Cmps (Just (repeat 1))
