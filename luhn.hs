module Luhn where

import Data.List
import Test.QuickCheck
import Common

checksum :: Integer -> Integer
checksum n = sum (digitsThatAre odd ++ concat [numberToDigits $ 2 * d | d <- digitsThatAre even])
  where digits = zip [1..] . reverse . numberToDigits $ n
        digitsThatAre f = [d | (i,d) <- digits, f i]

luhn :: Integer -> Bool
luhn n = (checksum n) `mod` 10 == 0

isAmericanExpress, isMasterCard, isVisa :: Integer -> Bool
isAmericanExpress n = luhn n && (length . numberToDigits $ n) == 15 && (elem (take 2 . numberToDigits $ n) [[3,4], [3,7]])
isMasterCard n = luhn n && (length . numberToDigits $ n) == 16 && ((head . numberToDigits $ n) == 5)
isVisa n = luhn n && (elem (length . numberToDigits $ n) [13, 16, 19]) && ((head . numberToDigits $ n) == 4)

validLuhns = [4012888888881881, 4111111111111111, 5274576394259961, 5555555555554444, 5105105105105100, 4222222222222, 371449635398431, 340000000000009]
invalidLuhns = [4012888688881881, 4111111111111181, 5274577394259961, 5555515555554444, 5105105205105100, 4222222252222, 371449635396431, 340000020000009]
luhnTests = all luhn validLuhns && all (not . luhn) invalidLuhns

-- *Luhn> quickCheck luhnTests
-- +++ OK, passed 1 tests.
