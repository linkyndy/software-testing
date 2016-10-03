module ClosureComposition where

import Test.QuickCheck
import Common
import SymmetricClosure
import TransitiveClosure

prop_composition :: Rel Int -> Bool
prop_composition r = (symmetricClosure . transitiveClosure $ r) == (transitiveClosure . symmetricClosure $ r)

-- *ClosureComposition> quickCheck prop_composition
-- *** Failed! Falsifiable (after 3 tests and 3 shrinks):
-- [(1,0)]
-- (0.00 secs, 515,072 bytes)

-- There is a difference between a symmetric closure of a transitive closure of
-- a relation r and vice-versa:
--
-- *ClosureComposition> symmetricClosure . transitiveClosure $ [(1,0)]
-- [(0,1),(1,0)]
-- *ClosureComposition> transitiveClosure . symmetricClosure $ [(1,0)]
-- [(0,0),(0,1),(1,0),(1,1)]
--
-- This simple example was found by using the above property and QuickCheck:
--
-- *ClosureComposition> verboseCheck prop_composition
-- [...]
-- Falsifiable (after 3 tests and 1 shrink):
-- [(1,0)]
