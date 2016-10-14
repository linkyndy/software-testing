module CarmichaelMillerRabinStrongLiars where

import System.IO.Unsafe
import Common

-- Finds Carmichael numbers that are a Miller-Rabin strong liars by checking k
-- random numbers for Miller-Rabin's Primality Test
carmichaelMillerRabinStrongLiars k = filter (unsafePerformIO . primeMR k) carmichael

-- *CarmichaelFermatLiars> take 10 $ carmichaelFermatLiars 1000
-- [13079177569,65700513721,100264053529,168003672409,172018713961,464052305161,527519713969,856666552249,1201586232601,1396066334401]
-- (1.02 secs, 440,877,960 bytes)
