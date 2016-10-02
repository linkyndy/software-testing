module ClosureTests where

import Data.List
import Test.QuickCheck
import Common
import SymmetricClosure
import TransitiveClosure

-- Check whether all pairs from both the relation and its inverse are in the
-- generated symmetric closure
prop_symmetric :: Rel Int -> Bool
prop_symmetric r = and [elem p (symmetricClosure r) | p <- nub (r ++ (inverseRelation r))]

-- Check that the generated symmetric closure has the same number of pairs as
-- the number of unique pairs from both the relation and its inverse
prop_cardinality :: Rel Int -> Bool
prop_cardinality r = length (symmetricClosure r) == length (nub (r ++ (inverseRelation r)))

-- Check that pairs from the initial relation are present in the generated
-- transitive closure
prop_initial_pairs :: Rel Int -> Bool
prop_initial_pairs r = and [elem p closure | p <- r]
  where closure = transitiveClosure r

-- Check whether the resulting relation is transitive
prop_transitive :: Rel Int -> Bool
prop_transitive r = and [elem (a,c) closure | (a,b) <- closure, (b',c) <- closure, b == b']
  where closure = transitiveClosure r
