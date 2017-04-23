module Task_3_3_9 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Data.Char
import Control.Monad

type MyRWT m = WriterT String (ReaderT [String] m)


runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt e = runReaderT (runWriterT rwt) e

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks f = lift $ asks f

myTell :: Monad m => String -> MyRWT m ()
myTell = tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

myAsk :: Monad m => MyRWT m [String]
myAsk = lift $ ask

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2
