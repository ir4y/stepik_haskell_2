module Task_3_3_11 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Char
import Control.Monad.State

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi m a = runState (runExceptT m) a


go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go low high state =
  (lift get) >>= \step ->
    let
      (low', high') = runState state step
    in do
      when (low  >= low')  (throwE "Lower bound")
      lift $ put high'
      when (high <= high') (throwE "Upper bound")
      return ()
