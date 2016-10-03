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

-- *SetAlgebra> quickCheck prop_intersection_with_empty
-- +++ OK, passed 100 tests.
-- (0.04 secs, 6,835,488 bytes)

prop_intersection_idempotence :: Set Int -> Bool
prop_intersection_idempotence a = intersectSet a a == a

-- *SetAlgebra> quickCheck prop_intersection_idempotence
-- +++ OK, passed 100 tests.
-- (0.02 secs, 9,045,016 bytes)

prop_intersection_commutativity :: Set Int -> Set Int -> Bool
prop_intersection_commutativity a b = intersectSet a b == intersectSet b a

-- *SetAlgebra> quickCheck prop_intersection_commutativity
-- +++ OK, passed 100 tests.
-- (0.02 secs, 13,986,568 bytes)

prop_intersection_associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_intersection_associativity a b c = intersectSet a (intersectSet b c) == intersectSet (intersectSet a b) c

-- *SetAlgebra> quickCheck prop_intersection_associativity
-- +++ OK, passed 100 tests.
-- (0.02 secs, 20,569,544 bytes)

prop_intersection_items :: Set Int -> Set Int -> Bool
prop_intersection_items a@(Set xs) b@(Set ys) = and [inSet x b --> inSet x intersection | x <- xs]
  where
    intersection = intersectSet a b

-- *SetAlgebra> quickCheck prop_intersection_items
-- +++ OK, passed 100 tests.
-- (0.03 secs, 17,926,368 bytes)

prop_union_with_empty :: Set Int -> Bool
prop_union_with_empty a = unionSet a emptySet == a

-- *SetAlgebra> quickCheck prop_union_with_empty
-- +++ OK, passed 100 tests.
-- (0.01 secs, 8,865,424 bytes)

prop_union_idempotence :: Set Int -> Bool
prop_union_idempotence a = unionSet a a == a

-- *SetAlgebra> quickCheck prop_union_idempotence
-- +++ OK, passed 100 tests.
-- (0.01 secs, 2,057,768 bytes)

prop_union_commutativity :: Set Int -> Set Int -> Bool
prop_union_commutativity a b = unionSet a b == unionSet b a

-- *SetAlgebra> quickCheck prop_union_commutativity
-- +++ OK, passed 100 tests.
-- (0.05 secs, 29,907,144 bytes)

prop_union_associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_union_associativity a b c = unionSet a (unionSet b c) == unionSet (unionSet a b) c

-- *SetAlgebra> quickCheck prop_union_associativity
-- +++ OK, passed 100 tests.
-- (0.11 secs, 78,286,464 bytes)

prop_union_items :: Set Int -> Set Int -> Bool
prop_union_items a@(Set xs) b@(Set ys) = and [inSet x union | x <- xs ++ ys]
  where
    union = unionSet a b

-- *SetAlgebra> quickCheck prop_union_items
-- +++ OK, passed 100 tests.
-- (0.05 secs, 28,438,504 bytes)

prop_difference_with_empty :: Set Int -> Bool
prop_difference_with_empty a = differenceSet a emptySet == a

-- *SetAlgebra> quickCheck prop_difference_with_empty
-- +++ OK, passed 100 tests.
-- (0.01 secs, 7,785,848 bytes)

prop_difference_of_empty :: Set Int -> Bool
prop_difference_of_empty a = differenceSet emptySet a == emptySet

-- *SetAlgebra> quickCheck prop_difference_of_empty
-- +++ OK, passed 100 tests.
-- (0.01 secs, 8,433,352 bytes)

prop_difference_with_self :: Set Int -> Bool
prop_difference_with_self a = differenceSet a a == emptySet

-- *SetAlgebra> quickCheck prop_difference_with_self
-- +++ OK, passed 100 tests.
-- (0.01 secs, 7,312,848 bytes)

prop_difference_items :: Set Int -> Set Int -> Bool
prop_difference_items a@(Set xs) b@(Set ys) = and [not (inSet x b) --> inSet x difference | x <- xs]
  where
    difference = differenceSet a b

-- *SetAlgebra> quickCheck prop_difference_items
-- +++ OK, passed 100 tests.
-- (0.03 secs, 19,147,528 bytes)

prop_distributivity_of_intersection :: Set Int -> Set Int -> Set Int -> Bool
prop_distributivity_of_intersection a b c = intersectSet a (unionSet b c) == unionSet (intersectSet a b) (intersectSet a c)

-- *SetAlgebra> quickCheck prop_distributivity_of_intersection
-- +++ OK, passed 100 tests.
-- (0.04 secs, 31,545,296 bytes)

prop_distributivity_of_union :: Set Int -> Set Int -> Set Int -> Bool
prop_distributivity_of_union a b c = unionSet a (intersectSet b c) == intersectSet (unionSet a b) (unionSet a c)

-- *SetAlgebra> quickCheck prop_distributivity_of_union
-- +++ OK, passed 100 tests.
-- (0.05 secs, 40,944,360 bytes)
