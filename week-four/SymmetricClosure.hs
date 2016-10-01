module SymmetricClosure where

import Data.List
import Data.Tuple
import Common

inverseRelation :: Rel a -> Rel a
inverseRelation r = map swap r

symmetricClosure :: Ord a => Rel a -> Rel a
symmetricClosure r = sort $ union r (inverseRelation r)
