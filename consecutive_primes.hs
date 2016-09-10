module ConsecutivePrimes where

import Data.List
import Test.QuickCheck
import Common

consecutivePrimes = takeWhile (\p -> isPrime(p + 1)) (scanl1 (*) primeNumbers)

-- *ConsecutivePrimes> consecutivePrimes
-- [2,6,30,210,2310]
-- (0.01 secs, 234,064 bytes)
