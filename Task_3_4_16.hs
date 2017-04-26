module Task_3_4_16 where
import Task_3_4_3
import Task_3_4_6
import Task_3_4_10
import Task_3_4_12

import Control.Monad.Trans.Class

instance MonadTrans (Arr2T e1 e2) where
  lift m = Arr2T $ \e1 e2 -> m

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return (f e1 e2)
