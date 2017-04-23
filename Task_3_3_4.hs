module Task_3_3_4 where
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Data.Char


logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1
  return el2
