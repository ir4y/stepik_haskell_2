module Task_2_4_10 where
import Task_2_3_6

fromList [x] = Un x
fromList (x1 : (x2 : xs)) = Bi x1 x2 (fromList xs)

toList = foldr (:) []


{- 2.3.10 -}
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC a b c = fromList (toList a ++ toList b ++ toList c)

{- 2.3.11 -}
concatOC :: OddC (OddC a) -> OddC a
concatOC = fromList . concat . toList . (fmap toList)
