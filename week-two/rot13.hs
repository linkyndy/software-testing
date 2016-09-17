module Rot13 where

import Data.Char
import Test.QuickCheck

-- ROT13 (or rotate by 13 places) is an algorithm that subtitutes each letter with
-- the 13th next letter in the alphabet. Because there are 26 letters in the regular
-- alphabet, if ROT13 is applied two times on an input it results in the same input.
-- So, ROT13(ROT13(string)) == string.

rot13Char :: Char -> Char
rot13Char c
  | elem (toLower c) ['a'..'m'] = chr (ord c + 13)
  | elem (toLower c) ['n'..'z'] = chr (ord c - 13)
  | otherwise = c

rot13 :: String -> String
rot13 s = map rot13Char s

testRot13 :: String -> Bool
testRot13 s = rot13 (rot13 s) == s

-- *Rot13> quickCheck testRot13
-- +++ OK, passed 100 tests.
-- (0.13 secs, 17,344,960 bytes)
