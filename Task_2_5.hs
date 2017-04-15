module Task_2_5 where
import Control.Applicative

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p = snd . runPrsEP p 0

instance Functor PrsEP where
  fmap f p = PrsEP fun where
    fun pos s = fmap (>>= \(a,s') -> return (f a, s')) (runPrsEP p pos s)

instance Applicative PrsEP where
  pure a = PrsEP fun where
    fun pos s = (pos, Right (a, s))
  pf <*> pv = PrsEP fun where
    fun pos s =
      let
        (pos', resF) = (runPrsEP pf pos s)
      in case resF of
          Left text -> (pos', Left text)
          Right (f, s') ->
            let
              (pos'', resV) = (runPrsEP pv pos' s')
            in case resV of
                Left text' -> (pos'', Left text')
                Right (v, s'') ->
                  (pos'', Right (f v, s''))


instance Alternative PrsEP where
  empty = PrsEP fun where 
    fun pos _ = (pos, Left ("pos " ++ (show pos) ++ ": empty alternative"))
  pl <|> pr = PrsEP fun where
    fun pos s =
      let
        (posL, resL) = (runPrsEP pl pos s)
        (posR, resR) = (runPrsEP pr pos s)
      in
        case (resL, resR) of
          ((Right d), _) -> (posL, (Right d))
          ((Left _), (Right d)) -> (posR, (Right d))
          ((Left el), (Left er)) -> if posL >= posR then
            (posL, (Left el))
            else
            (posR, (Left er))

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP p = PrsEP fun where
  fun pos []                 = (pos + 1, Left ("pos " ++ (show (pos + 1)) ++ ": unexpected end of input"))
  fun pos (c:cs) | p c       = (pos + 1, Right (c, cs))
                 | otherwise = (pos + 1, Left ("pos " ++ (show (pos + 1)) ++ ": unexpected " ++ (c:"")))

charEP c = satisfyEP (== c)

anyEP = satisfyEP (const True)

testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
