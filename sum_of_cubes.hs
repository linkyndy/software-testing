module SumOfCubes where

import Data.List
import Test.QuickCheck

cube_sum_for :: Integer -> Integer
cube_sum_for n = foldl1 (+) . map (^3) $ [0..n]

expected_sum_for :: Integer -> Integer
expected_sum_for n = (n * (n + 1) `div` 2) ^ 2

sum_of_cubes_test = \i -> map cube_sum_for [0..i] == map expected_sum_for [0..i]

-- *SumOfCubes> quickCheck sum_of_cubes_test
-- +++ OK, passed 100 tests.
