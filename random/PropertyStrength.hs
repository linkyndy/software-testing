module PropertyStrength where

import Data.List

stronger, weaker :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
stronger p q xs = all (\x -> not (p x) || q x) xs -- p implies q
weaker p q xs = stronger q p xs

strength :: (a -> Bool) -> (a -> Bool) -> [a] -> String
strength p q xs = check (stronger p q xs) (stronger q p xs)
  where
    check True True = "p and q are equivalent"
    check True _ = "p is stronger than q"
    check _ True = "p is weaker than q"
    check _ _ = "p and q are incomparable"

-- *PropertyStrength> stronger (\x -> x >= 0) (\x -> x <= 0) [-10..10]
-- False
-- *PropertyStrength> strength (\x -> x == -1) (\x -> x >= -1) [-10..10]
-- "p is stronger than q"
-- *PropertyStrength> strength (\x -> x < 3) (\x -> x /= 0) [-10..10]
-- "p and q are incomparable"
