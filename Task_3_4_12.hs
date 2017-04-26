module Task_3_4_12 where
import Task_3_4_3
import Task_3_4_6
import Task_3_4_10

instance Monad m => Monad (Arr2T e1 e2 m) where
  mv >>= f = Arr2T $ \e1 e2 -> do
    v <- getArr2T mv e1 e2
    getArr2T (f v) e1 e2
  fail res = Arr2T $ \e1 e2 -> fail res


instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  mv >>= f = Arr3T $ \e1 e2 e3 -> do
    v <- getArr3T mv e1 e2 e3
    getArr3T (f v) e1 e2 e3
  fail res = Arr3T $ \e1 e2 e3 -> fail res

a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
