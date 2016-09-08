module PowersetCardinality where

import Data.List
import Test.QuickCheck

powerset_cardinality_for :: [Integer] -> Int
powerset_cardinality_for = length . subsequences

expected_cardinality_for :: [Integer] -> Int
expected_cardinality_for = (2^) . length

powerset_cardinality_test = \i -> powerset_cardinality_for [0..i] == expected_cardinality_for [0..i]

-- The property is hard to test because as we increase the test iteration, it
-- becomes harder and harder to compute the powerset of lists having many elements
-- inside them.

-- The above tests mainly concern that subsequences' satisfies its specification.
-- We check that the function returns what we expect it to return, following a
-- mathematical formula
