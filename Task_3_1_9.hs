module Task_3_1_9 where
import Task_3_1_8
import Data.Monoid
import Text.Read
import Control.Monad.Trans.Except
import Control.Monad.Identity

data SumError = SumError Int ReadError
  deriving Show

-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps { getCmps :: f (g a)} deriving Show

-- instance Monoid a => Monoid (Except e a) where
--   mempty = return mempty
--   a `mappend` b = do
--     v1 <- a
--     v2 <- b
--     return $ v1 `mappend` v2

mappend' :: Monoid m => Except e m -> Except e m -> Except e m
l `mappend'` r = do
  ml <- l
  mr <- r
  return $ ml `mappend` mr

trySum :: [String] -> Except SumError Integer
trySum = (fmap getSum) . (foldr mappend' (return mempty)) . (fmap (fmap Sum)) . (parse 1)

parse :: Read a => Int -> [String] -> [Except SumError a]
parse _ [] = []
parse index (x:xs) = x' : (parse (index + 1) xs) where
    x' = withExcept (SumError index) $ (tryRead x)
