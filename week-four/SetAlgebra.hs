module SetAlgebra where

import Data.List
import Test.QuickCheck
import Common
import RandomSetGenerator
import SetOrd

intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = Set (intersect xs ys)

-- unionSet :: (Ord a) => Set a -> Set a -> Set a
-- unionSet (Set xs) (Set ys) = Set (union xs ys)

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = Set (xs \\ ys)

prop_intersection_with_empty :: Set Int -> Bool
prop_intersection_with_empty a = intersectSet a emptySet == emptySet

prop_intersection_idempotence :: Set Int -> Bool
prop_intersection_idempotence a = intersectSet a a == a

prop_intersection_commutativity :: Set Int -> Set Int -> Bool
prop_intersection_commutativity a b = intersectSet a b == intersectSet b a

prop_intersection_associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_intersection_associativity a b c = intersectSet a (intersectSet b c) == intersectSet (intersectSet a b) c

prop_intersection_items :: Set Int -> Set Int -> Bool
prop_intersection_items a@(Set xs) b@(Set ys) = and [inSet x b --> inSet x (intersectSet a b) | x <- xs]

prop_union_with_empty :: Set Int -> Bool
prop_union_with_empty a = unionSet a emptySet == a

prop_union_idempotence :: Set Int -> Bool
prop_union_idempotence a = unionSet a a == a

prop_union_commutativity :: Set Int -> Set Int -> Bool
prop_union_commutativity a b = unionSet a b == unionSet b a

prop_union_associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_union_associativity a b c = unionSet a (unionSet b c) == unionSet (unionSet a b) c

prop_union_items :: Set Int -> Set Int -> Bool
prop_union_items a@(Set xs) b@(Set ys) = and [inSet x (unionSet a b) | x <- xs ++ ys]

prop_difference_with_empty :: Set Int -> Bool
prop_difference_with_empty a = differenceSet a emptySet == a

prop_difference_of_empty :: Set Int -> Bool
prop_difference_of_empty a = differenceSet emptySet a == emptySet

prop_difference_with_self :: Set Int -> Bool
prop_difference_with_self a = differenceSet a a == emptySet

prop_difference_items :: Set Int -> Set Int -> Bool
prop_difference_items a@(Set xs) b@(Set ys) = and [not (inSet x b) --> inSet x (differenceSet a b) | x <- xs]

prop_distributivity_of_intersection :: Set Int -> Set Int -> Set Int -> Bool
prop_distributivity_of_intersection a b c = intersectSet a (unionSet b c) == unionSet (intersectSet a b) (intersectSet a c)

prop_distributivity_of_union :: Set Int -> Set Int -> Set Int -> Bool
prop_distributivity_of_union a b c = unionSet a (intersectSet b c) == intersectSet (unionSet a b) (unionSet a c)
