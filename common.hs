module Common where

factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]

is_prime :: Integer -> Bool
is_prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) prime_numbers

prime_numbers :: [Integer]
prime_numbers = 2 : filter is_prime [3..]

reverse_number :: Integer -> Integer
reverse_number = read . reverse . show
