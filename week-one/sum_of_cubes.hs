module SumOfCubes where

import Data.List
import Test.QuickCheck

cubeSumFor :: Integer -> Integer
cubeSumFor n = foldl1 (+) . map (^3) $ [0..n]

expectedSumFor :: Integer -> Integer
expectedSumFor n = (n * (n + 1) `div` 2) ^ 2

sumOfCubesTest = \i -> map cubeSumFor [0..i] == map expectedSumFor [0..i]

-- *SumOfCubes> quickCheck sumOfCubesTest
-- +++ OK, passed 100 tests.
-- (0.03 secs, 20,162,848 bytes)
