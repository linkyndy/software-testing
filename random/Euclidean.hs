module Euclidean where

euclidean :: Integer -> Integer -> Integer
euclidean a 0 = a
euclidean a b = euclidean b (rem a b)

-- *Euclidean> euclidean 15 20
-- 5
-- *Euclidean> euclidean 7 5
-- 1
-- *Euclidean> euclidean 10 4
-- 2
