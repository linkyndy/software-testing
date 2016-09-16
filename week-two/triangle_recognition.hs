module TriangleRecognition where

import Data.List
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | a' + b' < c'                = NoTriangle
  | a == b && b == c            = Equilateral
  | a == b || a == c || b == c  = Isosceles
  | a' ^ 2 + b' ^ 2 == c' ^ 2   = Rectangular
  | otherwise                   = Other
  where
    (a':b':c':[]) = sort (a:b:c:[])

fixtures =
  [
    ( NoTriangle
    ,
      [ [1, 1, 10]
      , [1, 10, 2]
      , [10, 2, 3]
      ]
    ,
      [ [1, 2, 3]
      , [1, 1, 2]
      , [1, 1, 1]
      ]
    )
  ,
    ( Equilateral
    ,
      [ [1, 1, 1]
      , [100, 100, 100]
      ]
    ,
      [ [1, 2, 3]
      , [1, 1, 2]
      , [1, 1, 10]
      ]
    )
  ,
    ( Isosceles
    , [ [1, 1, 2]
      , [1, 2, 1]
      , [2, 1, 1]
      , [100, 1, 100]
      ]
    ,
      [ [1, 1, 1]
      , [1, 2, 3]
      , [1, 1, 10] -- not a triangle
      ]
    )
  ,
    ( Rectangular
    , [ [3, 4, 5]
      ]
    ,
      [ [1, 1, 1]
      , [1, 1, 2]
      , [1, 2, 3]
      , [1, 1, 10]
      ]
    )
  ,
    ( Other
    , [ [1, 2, 3]
      ]
    ,
      [ [1, 1, 1]
      , [1, 1, 2]
      , [1, 1, 10]
      , [3, 4, 5]
      ]
    )
  ]
  
test = all (\(expected, valid, invalid) -> (all (\i -> triangle' i == expected) valid) && (all (\i -> triangle' i /= expected) invalid)) fixtures
  where
    triangle' (a:b:c:_) = triangle a b c
