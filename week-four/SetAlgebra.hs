module SetAlgebra where

import Data.List
import SetOrd

intersectSet :: Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = Set (intersect xs ys)

unionSet :: Set a -> Set a -> Set a
unionSet (Set xs) (Set ys) = Set (union xs ys)

differenceSet :: Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = Set (xs \\ ys)
