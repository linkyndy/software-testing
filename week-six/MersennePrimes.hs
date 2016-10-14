module MersennePrimes where

import System.IO.Unsafe
import Common

mersennePrimes = filter (unsafePerformIO . primeMR 1) (map (\p -> 2 ^ p - 1) primes)

-- *MersennePrimes> take 10 mersennePrimes
-- [3,7,31,127,8191,131071,524287,2147483647,2305843009213693951,618970019642690137449562111]
-- (0.04 secs, 1,603,552 bytes)
