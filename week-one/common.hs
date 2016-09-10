module Common where

factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) primeNumbers

primeNumbers :: [Integer]
primeNumbers = 2 : filter isPrime [3..]

reverseNumber :: Integer -> Integer
reverseNumber = read . reverse . show

numberToDigits :: Integer -> [Integer]
numberToDigits 0 = []
numberToDigits n = numberToDigits (n `div` 10) ++ [n `mod` 10]
