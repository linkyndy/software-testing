module SmallestPrime where

import Data.List
import Test.QuickCheck
import Common

findSmallestPrimeFrom (first:rest) = if condition then sumOf101Primes else findSmallestPrimeFrom rest
  where next100Primes = take 100 (filter (\n -> n > first) primeNumbers)
        sumOf101Primes = sum (first:next100Primes)
        condition = isPrime sumOf101Primes

smallestPrime = findSmallestPrimeFrom primeNumbers

-- *SmallestPrime> smallestPrime
-- 37447
-- (0.01 secs, 2,292,192 bytes)
