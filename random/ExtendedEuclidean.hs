module ExtendedEuclidean where

extendedEuclidean :: Integer -> Integer -> (Integer, Integer)
extendedEuclidean a 0 = (1, 0)
extendedEuclidean a b = let (q, r) = quotRem a b
                            (s, t) = extendedEuclidean b r
                        in (t, s - t * q)

-- *ExtendedEuclidean> extendedEuclidean 8 5
-- (2,-3)
-- *ExtendedEuclidean> extendedEuclidean 5 3
-- (-1,2)
