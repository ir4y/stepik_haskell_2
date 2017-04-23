module Task_3_3_7 where
myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell
