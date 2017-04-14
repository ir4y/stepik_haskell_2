module Task_1_4 where
import Control.Applicative
import Data.Char

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f p = Prs fun where
    fun s = do
      (a, s') <- (runPrs p s)
      return (f a, s')

anyChr :: Prs Char
anyChr = Prs f where
  f "" = Nothing
  f (c : cs) = Just (c, cs)

satisfy :: (Char -> Bool) -> Prs Char
satisfy p = Prs fun where
  fun [] = Nothing
  fun (c : cs) | p c = Just (c, cs)
             | otherwise = Nothing

char :: Char -> Prs Char
char c = satisfy (== c)

instance Applicative Prs where
  pure a = Prs fun where
    fun s = Just (a, s)
  pf <*> pv = Prs fun where
    fun s = do
      (f, s')  <- (runPrs pf s)
      (v, s'') <- (runPrs pv s')
      return (f v, s'')

instance Alternative Prs where
  empty = Prs fun where
    fun _ = Nothing
  p1 <|> p2 = Prs fun where
    fun s = (runPrs p1 s) <|> (runPrs p2 s)

many1 :: Prs a -> Prs [a]
many1 p = Prs fun where
  fun s = do
    (a, s') <- (runPrs p s)
    (as, s'') <- (runPrs (many p) s')
    return (a : as, s'')

digit :: Prs Int
digit = digitToInt <$> satisfy isDigit

nat :: Prs Int
nat = f <$> many1 digit where
  f = foldl (\a -> \b -> 10 * a + b ) 0

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f p = PrsE fun where
    fun s = do
      (a, s') <- (runPrsE p s)
      return (f a, s')

instance Applicative PrsE where
  pure a = PrsE fun where
    fun s = Right (a, s)
  pf <*> pv = PrsE fun where
    fun s = do
      (f, s')  <- (runPrsE pf s)
      (v, s'') <- (runPrsE pv s')
      return (f v, s'')

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE fun where
  fun [] = Left "unexpected end of input"
  fun (c:cs) | p c = Right (c, cs)
             | otherwise = Left ("unexpected " ++ (c:""))

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
