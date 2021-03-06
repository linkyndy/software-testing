module Common where

import Data.Function
import Data.List
import Data.Tuple

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

type Rel a = [(a, a)]

inverseRelation :: Rel a -> Rel a
inverseRelation r = map swap r

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

fp :: (Eq a) => (a -> a) -> a -> a
fp f = fix (\g x -> if x == f x then x else g (f x))
