module SymmetricClosure where

import Data.List
import Common

symmetricClosure :: Ord a => Rel a -> Rel a
symmetricClosure r = sort . nub $ union r (inverseRelation r)

-- *SymmetricClosure> symmetricClosure [(1,2),(2,3),(3,4)]
-- [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
-- (0.02 secs, 128,016 bytes)
