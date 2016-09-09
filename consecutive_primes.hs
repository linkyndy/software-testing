module ConsecutivePrimes where

import Data.List
import Test.QuickCheck
import Common

consecutive_primes = takeWhile (\p -> is_prime(p + 1)) (scanl1 (*) prime_numbers)

-- *ConsecutivePrimes> consecutive_primes
-- [2,6,30,210,2310]
