module Task_3_4_10 where
import Task_3_4_3
import Task_3_4_6

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure x = Arr2T $ \e1 e2 -> pure x
  f <*> g = Arr2T $ \e1 e2 -> (getArr2T f e1 e2) <*> (getArr2T g e1 e2)

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure x = Arr3T $ \e1 e2 e3 -> pure x
  f <*> g = Arr3T $ \e1 e2 e3 -> (getArr3T f e1 e2 e3) <*> (getArr3T g e1 e2 e3)
