module Task_3_3_12 where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a
                 -> (Integer,Integer)
                 -> Integer
                 -> m (Either String a, Integer)
runRiiEsSiT m e a = runStateT (runExceptT (runReaderT m e)) a

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go state = do
  lift.lift $ state
  v <- lift.lift $ get
  (low, high) <- ask
  when (low  >= v)  (lift $ throwE "Lower bound")
  when (high <= v) (lift $ throwE "Upper bound")
  return ()


tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n
