module Task_3_2_6 where

showCont :: Show a => Cont String a -> String
showCont m = runCont m show
