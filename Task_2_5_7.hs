module Task_2_5_7 where

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP p = PrsEP fun where
  fun pos []                 = (pos + 1, Left ("pos " ++ (show (pos + 1)) ++ ": unexpected end of input"))
  fun pos (c:cs) | p c       = (pos + 1, Right (c, cs))
                 | otherwise = (pos + 1, Left ("pos " ++ (show (pos + 1)) ++ ": unexpected " ++ (c:"")))

