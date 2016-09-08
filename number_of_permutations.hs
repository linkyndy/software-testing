module NumberOfPermutations where

import Data.List
import Test.QuickCheck
import Common

permutations_number_for :: [Integer] -> Int
permutations_number_for = length . permutations

expected_number_for :: [Integer] -> Int
expected_number_for = factorial . length

number_of_permutations_test = \i -> permutations_number_for [0..i] == expected_number_for [0..i]

-- The property is hard to test because as we increase the test iteration, it
-- becomes harder and harder to compute permutations of lists having many elements
-- inside them.

-- The above tests mainly concern that permutations' satisfies its specification.
-- We check that the function returns what we expect it to return
