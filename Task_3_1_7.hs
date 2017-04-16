module Task_3_1_7 where
import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

check []     index pos = True
check (x:xs) index pos | pos >= index = False
                       | otherwise    = check xs index (pos + 1)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
array !!! index | 0 > index           = throwE ErrNegativeIndex
                | check array index 0 = throwE $ ErrIndexTooLarge index
                | otherwise           = return $ array !! index

infixl 9 !!!!
(!!!!) xs n = runExcept $ xs !!! n
