{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Task_3_1_3 where
import Data.Bifunctor

newtype Except e a = Except {runExcept :: Either e a}
                     deriving (Show, Bifunctor, Functor, Applicative, Monad)

except :: Either e a -> Except e a
except = Except


withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = first

-- withExcept :: (e -> e') -> Except e a -> Except e' a
-- withExcept f (Except either) = (Except (first f either))
