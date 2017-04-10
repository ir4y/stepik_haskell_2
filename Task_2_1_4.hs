module Task_2_1_4 where

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
  foldr f init (Tr a b c) = foldr f init [a, b, c]
  foldl f init (Tr a b c) = foldl f init [a, b, c]
