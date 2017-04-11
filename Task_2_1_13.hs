module Task_2_1_13 where
import Data.Monoid
mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo
