module FermatPrimalityCheck where

import Data.List
import System.IO.Unsafe
import Common

-- Finds the least composite that is a Fermat liar by checking k random numbers
-- for Fermat's Primality Test
leastFermatLiar k = find (unsafePerformIO . primeTestsF k) composites

-- Executes leastFermatLiar n times and gets the absolute minimum Fermat liar
-- that is found
minimumOfLeastFermatLiars n k = minimum <$> sequence leastFermatLiars
  where leastFermatLiars = (\_ -> leastFermatLiar k) <$> [1..n]

-- *FermatPrimalityCheck> minimumOfLeastFermatLiars 1000 1
-- Just 9
-- (0.29 secs, 125,403,064 bytes)
-- *FermatPrimalityCheck> minimumOfLeastFermatLiars 1000 2
-- Just 9
-- (5.97 secs, 2,880,922,272 bytes)
-- *FermatPrimalityCheck> minimumOfLeastFermatLiars 1000 3
-- Just 9
-- (28.16 secs, 14,357,720,064 bytes)
-- *FermatPrimalityCheck> minimumOfLeastFermatLiars 1000 4
-- Just 9
-- (62.09 secs, 33,224,261,536 bytes)
-- *FermatPrimalityCheck> minimumOfLeastFermatLiars 1000 5
-- Just 15
-- (122.24 secs, 68,025,224,512 bytes)
-- *FermatPrimalityCheck> minimumOfLeastFermatLiars 1000 7
-- Just 45
-- (421.53 secs, 248,043,548,776 bytes)

-- The least Fermat Liar that could be found was 9. Increasing k (the number of
-- random numbers used for Fermat's Primality Test) increases the chance for a
-- Fermat liar to be spotted and marked accordingly. That's why for k > 4 number
-- 9 wasn't reported as a prime anymore by Fermat's Primality Test.
