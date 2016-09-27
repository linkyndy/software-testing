module ParserTest where

import Test.QuickCheck
import Common
import RandomFormulaGenerator

testParser f = (head . parse . show $ f) == f
