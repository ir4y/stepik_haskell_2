module Task_3_2_10 where
import Task_3_1_8
import Control.Monad
import Control.Monad.Trans.Except

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Except e a -> FailCont r e a
toFailCont data' = case (runExcept data') of
  (Left x) -> FailCont $ \_ e -> e x
  (Right x) -> FailCont $ \c _ -> c x

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m Right Left

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

instance Monad (FailCont r e) where
  return x = FailCont $ \c e -> c x
  FailCont v >>= k = FailCont $ \c e -> v (\a -> runFailCont (k a) c e) e

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2
