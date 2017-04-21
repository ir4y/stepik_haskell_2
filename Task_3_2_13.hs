module Task_3_2_13 where
import Task_3_2_10
import Task_3_2_9

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \c -> runCont (f $ \a -> Cont $ \_ -> c a) c

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \c e -> runFailCont (f $ \a -> FailCont $ \_ _ -> c a) c e

