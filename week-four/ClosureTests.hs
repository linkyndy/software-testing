module ClosureTests where

import Data.List
import Test.QuickCheck
import Common
import SymmetricClosure
import TransitiveClosure

-- Check whether all pairs from both the relation and its inverse are in the
-- generated symmetric closure (i.e. resulting relation is symmetric)
prop_symmetric :: Rel Int -> Bool
prop_symmetric r = and [elem p closure | p <- nub (r ++ (inverseRelation r))]
  where closure = symmetricClosure r

-- *ClosureTests> quickCheck prop_symmetric
-- +++ OK, passed 100 tests.
-- (0.12 secs, 25,100,680 bytes)

-- Check that the generated symmetric closure has the same number of pairs as
-- the number of unique pairs from both the relation and its inverse
prop_cardinality :: Rel Int -> Bool
prop_cardinality r = length closure == length (nub (r ++ (inverseRelation r)))
  where closure = symmetricClosure r

-- *ClosureTests> quickCheck prop_cardinality
-- +++ OK, passed 100 tests.
-- (0.03 secs, 20,794,992 bytes)`

-- Check that pairs from the initial relation are present in the generated
-- transitive closure
prop_initial_pairs :: Rel Int -> Bool
prop_initial_pairs r = and [elem p closure | p <- r]
  where closure = transitiveClosure r

-- *ClosureTests> quickCheck prop_initial_pairs
-- +++ OK, passed 100 tests.
-- (0.29 secs, 79,801,832 bytes)

-- Check whether the resulting relation is transitive
prop_transitive :: Rel Int -> Bool
prop_transitive r = and [elem (a,c) closure | (a,b) <- closure, (b',c) <- closure, b == b']
  where closure = transitiveClosure r

-- *ClosureTests> quickCheck prop_transitive
-- +++ OK, passed 100 tests.
-- (0.36 secs, 88,046,456 bytes)
