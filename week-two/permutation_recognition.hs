module PermutationRecognition where

import Data.List
import Test.QuickCheck

isPermutation :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

-- *PermutationRecognition> isPermutation [0..10000] [10000, 9999..0]
-- True
-- (0.03 secs, 2,336,608 bytes)

-- Other solutions (much slower):
--  isPermutation' xs ys = length xs == length ys && length (intersect xs ys) == length xs
--    (1.26 secs, 2,810,216 bytes)
--  isPermutation' xs ys = null (xs \\ ys) && null (ys \\ xs)
--    (5.22 secs, 10,537,621,056 bytes)

testDifferentLength :: Integer -> Property
testDifferentLength n = n >= 0 ==> not (isPermutation [0..n] [1..n])

-- *PermutationRecognition> quickCheck testDifferentLength
-- +++ OK, passed 100 tests.
-- (0.11 secs, 3,454,512 bytes)

testSameList :: Integer -> Property
testSameList n = n >= 0 ==> isPermutation [0..n] [0..n]

-- *PermutationRecognition> quickCheck testSameList
-- +++ OK, passed 100 tests.
-- (0.11 secs, 3,684,760 bytes)

testSameElements :: Integer -> Property
testSameElements n = n >= 0 ==> isPermutation [0..n] [n, n-1..0]

-- *PermutationRecognition> quickCheck testSameElements
-- +++ OK, passed 100 tests.
-- (0.11 secs, 3,478,248 bytes)

testDifferentElements :: Integer -> Property
testDifferentElements n = n >= 0 ==> not (isPermutation [0..n] [1..n+1])

-- *PermutationRecognition> quickCheck testDifferentElements
-- +++ OK, passed 100 tests.
-- (0.11 secs, 3,248,776 bytes)
