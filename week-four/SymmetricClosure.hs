module SymmetricClosure where

import Data.List
import Common

symmetricClosure :: Ord a => Rel a -> Rel a
symmetricClosure r = sort . nub $ union r (inverseRelation r)
