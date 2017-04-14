module Task_2_4_7 where
import Task_1_4

instance Monad PrsE where
  p >>= f = PrsE fun where
    fun s = do
      (d, s') <- (runPrsE p s)
      runPrsE (f d) s'
