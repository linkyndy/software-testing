module NumberOfPermutations where

import Data.List
import Test.QuickCheck
import Common

permutationsNumberFor :: [Integer] -> Int
permutationsNumberFor = length . permutations

expectedNumberFor :: [Integer] -> Int
expectedNumberFor = factorial . length

numberOfPermutationsTest = \i -> permutationsNumberFor [0..i] == expectedNumberFor [0..i]

-- The property is hard to test because as we increase the test iteration, it
-- becomes harder and harder to compute permutations of lists having many elements
-- inside them.

-- The above tests mainly concern that permutations' satisfies its specification.
-- We check that the function returns what we expect it to return
