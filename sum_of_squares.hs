module SumOfSquares where

import Data.List
import Test.QuickCheck

square_sum_for :: Integer -> Integer
square_sum_for n = foldl1 (+) . map (^2) $ [0..n]

expected_sum_for :: Integer -> Integer
expected_sum_for n = n * (n + 1) * (2 * n + 1) `div` 6

sum_of_squares_test = \i -> map square_sum_for [0..i] == map expected_sum_for [0..i]

-- *SumOfSquares> quickCheck sum_of_squares_test
-- +++ OK, passed 100 tests.
