module Task_4_1 where
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Class

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (LoggT m) where
  return v = LoggT $ return (Logged "" v)
  m >>= k = LoggT $ do
    (Logged str  v ) <- runLoggT m
    (Logged str' v') <- runLoggT (k v)
    return $ Logged (str ++ str') v'
  fail = LoggT . fail


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity .runLoggT

instance MonadTrans LoggT where
  lift m = LoggT $ do
    v <- m
    return $ Logged "" v 
