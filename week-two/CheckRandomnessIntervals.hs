module CheckRandomnessIntervals where

import Data.List
import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)

-- Partitions a list of numbers in 4 sublists by their belonging in one of the
-- intervals defined by 0.25, 0.5 and 0.75
getPartitionsFor :: [Float] -> [[Float]]
getPartitionsFor numbers = q1:q2:q3:q4:[]
  where
    halves = partition (< 0.5) numbers
    (q1, q2) = partition (< 0.25) (fst halves)
    (q3, q4) = partition (< 0.75) (snd halves)

-- Checks whether each of the (four) given partitions has roughly the same number
-- of elements, by dividing the total n number of partitioned elements by 4 and
-- applying a thrshld percent threshold
checkProportions :: Float -> Float -> [[Float]] -> Bool
checkProportions n thrshld partitions = all condition partitions
  where
    lengthFor = fromIntegral . length
    lowerBound = round $ n / 4 - n * thrshld / 100
    upperBound = round $ n / 4 + n * thrshld / 100
    condition p = lengthFor p >= lowerBound && lengthFor p <= upperBound

main :: IO Bool
main = do
  numbers <- probs 10
  let partitions = getPartitionsFor numbers
      isTrueRandom = checkProportions 10 50 partitions
  return isTrueRandom

-- The statement is not entirely true. The smaller the amount of generated
-- numbers, the bigger the threshold should be in order for this claim to hold,
-- that is, generated numbers should be random in the (0..1) interval. We can
-- observe that a threshold of 0.5% is enough for 1_000_000 generated numbers,
-- while for 10 generated numbers we need a threshold of around 50% for the
-- claim to hold.
--
-- +----------+---------+--------------+
-- | n        | thrshld | isTrueRandom |
-- +----------+---------+--------------+
-- | 10       | 50      | True         |
-- | 10       | 20      | True/False   |
-- | 10       | 10      | True/False   |
-- | 100      | 10      | True         |
-- | 100      | 5       | True/False   |
-- | 1000     | 5       | True         |
-- | 1000     | 2       | True/False   |
-- | 1000     | 0.5     | False        |
-- | 1000000  | 2       | True         |
-- | 1000000  | 0.5     | True         |
-- | 1000000  | 0.05    | False        |
-- +----------+---------+--------------+
