module Task_3_3_10 where
import Task_3_3_9
import Data.Char

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  evens <- myAsks $ filter (even . length)
  odds  <- myAsks $ filter (odd .  length)
  case (evens, odds) of
    ((e1:e2:_), (o1:o2:_)) -> do
        myTell $ e1 ++ "," ++ o1
        return $ (toUpper <$> e2,toUpper <$> o2)
    _ -> myLift Nothing
