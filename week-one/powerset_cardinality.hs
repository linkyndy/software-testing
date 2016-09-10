module PowersetCardinality where

import Data.List
import Test.QuickCheck

powersetCardinalityFor :: [Integer] -> Int
powersetCardinalityFor = length . subsequences

expectedCardinalityFor :: [Integer] -> Int
expectedCardinalityFor = (2^) . length

powersetCardinalityTest = \i -> powersetCardinalityFor [0..i] == expectedCardinalityFor [0..i]

-- The property is hard to test because as we increase the test iteration, it
-- becomes harder and harder to compute the powerset of lists having many elements
-- inside them.

-- The above tests mainly concern that subsequences' satisfies its specification.
-- We check that the function returns what we expect it to return, following a
-- mathematical formula
