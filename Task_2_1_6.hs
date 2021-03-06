module Task_2_1_6 where
import Data.List

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Tree where
  foldr f init Nil = init
  foldr f init (Branch l x r) = (foldr f (f x (foldr f init r)) l)

 {- For task 2.2.4 -}
instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l x r) = (Branch (fmap f l) (f x) (fmap f r))

instance Foldable Preorder where
  foldr f init (PreO Nil) = init
  foldr f init (PreO (Branch l x r)) = (f x (foldr f (foldr f init (PreO r)) (PreO l)))

 {- For task 2.2.4 -}
instance Functor Preorder where
  fmap f (PreO tree) = (PreO (fmap f tree))

instance Foldable Postorder where
  foldr f init (PostO Nil) = init
  foldr f init (PostO (Branch l x r)) = (foldr f (foldr f (f x init) (PostO r)) (PostO l))

instance Foldable Levelorder where
  foldr f init (LevelO tree) = foldr f init list where
    list = map snd (sortOn fst (toList tree))

toList :: Tree a -> [(Integer, a)]
toList tree = toLevelList 0 tree

toLevelList :: Integer -> Tree a -> [(Integer, a)]
toLevelList level Nil = []
toLevelList level (Branch l x r) =
  [(level, x)] ++ (toLevelList (level+1) l) ++ (toLevelList (level+1) r)

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
{-
    3
   / \
  1   4
   \
    2
-}


tree2 = Branch (Branch (Branch Nil 1 Nil) 2 (Branch (Branch (Branch Nil 8 Nil) 7 Nil) 3 (Branch Nil 6 Nil))) 4 (Branch Nil 5 (Branch Nil 9 (Branch Nil 10 (Branch Nil 11 Nil))))
{-
    4
   / \
  2   5
 / \   \
1   3   9
   / \   \
  7   6   10
 /         \
8           11
-}


{- For task 2.2.4 -}
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

{- For task 2.2.4 -}
tree3 = (PreO (Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))))
{-
    4
   / \
  2   5
 / \
1   3
-}

{- For task 2.2.5 -}
traverse_ :: (Foldable t, Applicative f) => (a -> (f b)) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())
