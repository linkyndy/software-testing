module DerangementRecognition where

import Data.List
import Test.QuickCheck

isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isDerangement xs ys = sort xs == sort ys && all (\(x, y) -> x /= y) (zip xs ys)

-- *DerangementRecognition> isDerangement [0..10000] [10000, 9999..0]
-- False
-- (0.04 secs, 3,338,624 bytes)

derangements :: (Eq a, Ord a) => [a] -> [[a]]
derangements xs = filter (\p -> isDerangement xs p) (permutations xs)

testDifferentLength :: Integer -> Property
testDifferentLength n = n >= 0 ==> not (isDerangement [0..n] [1..n])

-- *DerangementRecognition> quickCheck testDifferentLength
-- +++ OK, passed 100 tests.
-- (0.07 secs, 3,518,680 bytes)

testSameList :: Integer -> Property
testSameList n = n >= 0 ==> not (isDerangement [0..n] [0..n])

-- *DerangementRecognition> quickCheck testSameList
-- +++ OK, passed 100 tests.
-- (0.07 secs, 3,678,936 bytes)

testSameElementsEvenLength :: Integer -> Property
testSameElementsEvenLength n = n > 0 && even n ==> isDerangement [1..n] [n, n-1..1]

-- *DerangementRecognition> quickCheck testSameElementsEvenLength
-- +++ OK, passed 100 tests.
-- (0.09 secs, 8,555,312 bytes)

testSameElementsOddLength :: Integer -> Property
testSameElementsOddLength n = n > 0 && odd n ==> not (isDerangement [1..n] [n, n-1..1])

-- *DerangementRecognition> quickCheck testSameElementsOddLength
-- +++ OK, passed 100 tests.
-- (0.08 secs, 7,976,416 bytes)

testDifferentElements :: Integer -> Property
testDifferentElements n = n >= 0 ==> not (isDerangement [0..n] [1..n+1])

-- *DerangementRecognition> quickCheck testDifferentElements
-- +++ OK, passed 100 tests.
-- (0.07 secs, 3,556,168 bytes)
