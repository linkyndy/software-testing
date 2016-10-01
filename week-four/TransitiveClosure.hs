module TransitiveClosure where

import Data.List
import Common

transitiveClosure :: Ord a => Rel a -> Rel a
transitiveClosure r = sort $ fp ((union r) . (r @@)) r

-- The above could be expanded to: fp (\r -> union r (r @@ r)) r

-- *TransitiveClosure> transitiveClosure [(1,2), (2,3), (3,4)]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-- (0.02 secs, 138,472 bytes)
