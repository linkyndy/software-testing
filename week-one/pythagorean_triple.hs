module PythagoreanTriple where

import Data.List
import Test.QuickCheck
import Common

pythagoreanTriple = head [a * b * c | c <- [5..], b <- [4..(c-1)], a <- [3..(b-1)], a + b + c == 1000, a^2 + b^2 == c^2]

-- *PythagoreanTriple> pythagoreanTriple
-- 31875000
-- (7.51 secs, 3,538,840,800 bytes)

-- the numbers are 200, 375 and 425
