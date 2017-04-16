{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Task_3_1_14 where
import Task_3_1_8
import Task_3_1_9
import Control.Monad.Trans.Except
import Data.Monoid

newtype Validate e a = Validate { getValidate :: Either [e] a }
                       deriving (Show, Functor, Applicative, Monad)

collectE :: Except e a -> Validate e a
collectE err = case (runExcept err) of
  (Left e)  -> Validate (Left [e])
  (Right r) -> Validate (Right r)

instance Monoid a => Monoid (Validate e a) where
  mempty = Validate (Right mempty)
  (Validate (Left l)) `mappend` (Validate (Left r))  = (Validate (Left (l `mappend` r)))
  (Validate (Left l)) `mappend` (Validate (Right _)) = (Validate (Left l))
  (Validate (Right _)) `mappend` (Validate (Left r)) = (Validate (Left r))
  (Validate (Right l)) `mappend` (Validate (Right r)) = (Validate (Right (l `mappend` r)))

parse' :: Read a => Int -> [String] -> [Validate SumError a]
parse' _ [] = []
parse' index (x:xs) = x' : (parse' (index + 1) xs) where
    x' = collectE (withExcept (SumError index) $ (tryRead x))


validateSum :: [String] -> Validate SumError Integer
validateSum = (fmap getSum) . mconcat . (fmap (fmap Sum)) . (parse' 1)
