module SummationOfPrimes where

import Data.List
import Test.QuickCheck
import Common

summationOfPrimes = sum (takeWhile (<2000000) prime_numbers)

-- *SummationOfPrimes> summationOfPrimes 
-- 142913828922
