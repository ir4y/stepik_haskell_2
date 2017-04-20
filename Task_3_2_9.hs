module Task_3_2_9 where
import Control.Monad

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

showCont :: Show a => Cont String a -> String
showCont m = runCont m show

evalCont :: Cont a a -> a
evalCont m = runCont m id

instance Functor (Cont r) where
  fmap = liftM

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  return x = Cont $ \c -> c x
  Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)

type Checkpointed a = (a -> (Cont [a] a)) -> (Cont [a] a)

addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

safeFilter pred lst = case (filter pred lst) of
  [] -> [head lst]
  res -> res

checkpointed :: (a -> Bool) -> Checkpointed a -> Cont r a
checkpointed pred checkpoints = Cont $ \c' ->
  c' $ val where
    val = (head . reverse . (safeFilter pred))(runCont (checkpoints $ \v -> Cont $ \c -> (v : (c v))) (\x -> [x]))

doEval = \_ -> evalCont (checkpointed  (< 20) $ addTens 1)
