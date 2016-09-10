module SummationOfPrimes where

import Data.List
import Test.QuickCheck
import Common

summationOfPrimes = sum (takeWhile (<2000000) primeNumbers)

-- *SummationOfPrimes> summationOfPrimes
-- 142913828922
-- (40.50 secs, 26,264,522,528 bytes)
