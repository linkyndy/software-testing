module Common where

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
