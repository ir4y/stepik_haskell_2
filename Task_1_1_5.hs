module Task_1_1_5 where

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f1 (Arr2 f2 ) = (Arr2 (\ee1 -> \ee2 -> f1 (f2 ee1 ee2)))


newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr3 e1 e2 e3) where
  fmap f1 (Arr3 f2) = (Arr3 (\ee1 -> \ee2 -> \ee3 -> f1 (f2 ee1 ee2 ee3)))
