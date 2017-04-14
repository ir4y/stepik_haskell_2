module Task_2_3_12 where
import Data.Traversable (foldMapDefault, fmapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Traversable Tree where
  sequenceA Nil = pure Nil
  sequenceA (Branch l x r) =
    let
      l' = sequenceA l
      r' = sequenceA r
      branch l r x = Branch l x r 
    in
      branch <$> l' <*> r' <*> x

instance Foldable Tree where
  foldMap = foldMapDefault

instance Functor Tree where
  fmap = fmapDefault

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 (Branch (Branch Nil 7 Nil) 6 (Branch Nil 8 Nil)))
{-

    4
   / \
  2   5
 / \   \
1   3   6
       / \
      7   8
[1,2,3,4,5,6,7,8]
[1,3,2,7,8,6,5,4] -- post order

-}
