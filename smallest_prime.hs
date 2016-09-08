module SmallestPrime where

import Data.List
import Test.QuickCheck
import Common

find_smallest_prime_from (first:rest) = if condition then sum_of_101_primes else find_smallest_prime_from rest
  where next_100_primes = take 100 (filter (\n -> n > first) prime_numbers)
        sum_of_101_primes = sum (first:next_100_primes)
        condition = is_prime sum_of_101_primes

smallest_prime = find_smallest_prime_from prime_numbers

-- *SmallestPrime> smallest_prime
-- 37447
