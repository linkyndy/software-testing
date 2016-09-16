module PermutationRecognition where

import Data.List
import Test.QuickCheck

isPermutation :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation a b = sort a == sort b

-- *PermutationRecognition> isPermutation [0..10000] [10000, 9999..0]
-- True
-- (0.03 secs, 2,336,608 bytes)

-- Other solutions (much slower):
--  isPermutation' a b = length a == length b && length (intersect a b) == length a
--    (1.26 secs, 2,810,216 bytes)
--  isPermutation' a b = null (a \\ b) && null (b \\ a)
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

-- +++ OK, passed 100 tests.
-- (0.11 secs, 3,478,248 bytes)

testDifferentElements :: Integer -> Property
testDifferentElements n = n >= 0 ==> not (isPermutation [0..n] [1..n+1])

-- *PermutationRecognition> quickCheck testDifferentElements
-- +++ OK, passed 100 tests.
-- (0.11 secs, 3,248,776 bytes)
