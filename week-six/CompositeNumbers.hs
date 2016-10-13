module CompositeNumbers where

import Common

compositeNumbers :: [Integer]
compositeNumbers = filter (not . prime) [4..]

-- *CompositeNumbers> take 10 compositeNumbers
-- [4,6,8,9,10,12,14,15,16,18]
-- (0.03 secs, 154,128 bytes)
