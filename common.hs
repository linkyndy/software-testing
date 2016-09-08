module Common where

factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]
