module Task_3_4_6 where
import Task_3_4_3

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap f m = Arr2T (\e1 e2 -> fmap f ((getArr2T m) e1 e2))

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap f m = Arr3T (\e1 e2 e3 -> fmap f ((getArr3T m) e1 e2 e3))
