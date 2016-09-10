module SumOfSquares where

import Data.List
import Test.QuickCheck

squareSumFor :: Integer -> Integer
squareSumFor n = foldl1 (+) . map (^2) $ [0..n]

expectedSumFor :: Integer -> Integer
expectedSumFor n = n * (n + 1) * (2 * n + 1) `div` 6

sumOfSquaresTest = \i -> map squareSumFor [0..i] == map expectedSumFor [0..i]

-- *SumOfSquares> quickCheck sumOfSquaresTest
-- +++ OK, passed 100 tests.
-- (0.03 secs, 23,695,528 bytes)
