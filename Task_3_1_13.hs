{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Task_3_1_13 where
-- import Control.Monad
data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex

newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show, Monoid)

-- instance Monoid SimpleError where
--   mempty = Simple ""
--   (Simple l) `mappend` (Simple r) = Simple (l ++ r)

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge index) = Simple ("[index (" ++ show(index) ++ ") is too large]")
lie2se ErrNegativeIndex = Simple "[negative index]"
