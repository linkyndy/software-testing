module ModularExponentiation where

import Data.Bits
import Test.QuickCheck
import Common

modExp' :: Integer -> Integer -> Integer -> Integer
modExp' _ _ 1 = 0
modExp' _ 0 _ = 1
modExp' b e m = r * modExp' (b * b `mod` m) (shiftR e 1) m `mod` m
  where r = if testBit e 0 then b else 1

modExp :: Integer -> Integer -> Integer -> Integer
modExp b e m = modExp' b' e' m
  where
    -- If the exponent is negative, we use its absolute value and the modular
    -- multiplicative inverse of the base
    b' = if e < 0 then fst $ extendedEuclidean b m else b
    e' = abs e

-- NOTE: This doesn't test cases involving negative exponents
prop_validExponentiation :: Integer -> Integer -> Integer -> Property
prop_validExponentiation b e m = m > 0 && e > 0 ==> modExp b e m == b ^ e `mod` m

-- *ModularExponentiation> quickCheck prop_validExponentiation
-- +++ OK, passed 100 tests.
-- (0.20 secs, 9,567,712 bytes)

prop_validResultInterval :: Integer -> Integer -> Integer -> Property
prop_validResultInterval b e m = m > 0 ==> result >= 0 && result < m
  where result = modExp b e m

-- *ModularExponentiation> quickCheck prop_validResultInterval
-- +++ OK, passed 100 tests.
-- (0.01 secs, 5,617,696 bytes)
