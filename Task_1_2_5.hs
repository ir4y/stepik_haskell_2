module Task_1_2_5 where
import Control.Applicative (ZipList(ZipList), getZipList)

l1 >*< l2 = getZipList ((ZipList l1) <*>  (ZipList l2))
f >$< l1 = getZipList (f <$> (ZipList l1))
