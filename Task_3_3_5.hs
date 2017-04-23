module Task_3_3_5 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Data.Char
import Control.Monad

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 lst = separate' p1 p2 lst []


separate' :: (a -> Bool) -> (a -> Bool) -> [a] -> [a] -> WriterT [a] (Writer [a]) [a]
separate' _p1 _p2 []     acc = return acc
separate'  p1  p2 (x:xs) acc = do
  when (p1 x) (tell [x])
  when (p2 x) (lift $ tell [x])
  if (p1 x || p2 x) then
    separate' p1 p2 xs acc
  else
    separate' p1 p2 xs (acc ++ [x])
