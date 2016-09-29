module RandomSetGenerator where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

--
-- Generating sets the manual way...
--

generateSet :: IO (Set Int)
generateSet = do
  cardinality <- randomRIO (0, 1000)
  set <- populateSet emptySet cardinality
  return (set)

-- Receives an initial `s` set and populates it with `n` random items
populateSet :: Set Int -> Int -> IO (Set Int)
populateSet s 0 = do
  return (s)
populateSet s n = do
  item <- randomRIO (0, 1000)
  set <- populateSet (insertSet item s) (n - 1)
  return (set)

--
-- ...and the QuickCheck way!
--

-- Generates an ordered list of unique elements
uniqueOrderedList :: (Ord a, Arbitrary a) => Gen [a]
uniqueOrderedList = fmap (sort . nub) arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
    items <- uniqueOrderedList
    return (Set items)
