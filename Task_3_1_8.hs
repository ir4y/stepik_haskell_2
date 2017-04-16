module Task_3_1_8 where
import Text.Read
import Control.Monad.Trans.Except

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead input = case readMaybe input of
  Nothing  -> throwE $ NoParse input
  Just res -> return res
