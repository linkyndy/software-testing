module ParserTest where

import Common

validExpressions =
  [ ("1", Prop 1)
  , ("-1", Neg (Prop 1))
  , ("*(1)", Cnj [Prop 1])
  , ("*(-1)", Cnj [Neg (Prop 1)])
  , ("*(1 2)", Cnj [Prop 1, Prop 2])
  , ("*(1 -2)", Cnj [Prop 1, Neg (Prop 2)])
  , ("*(-1 2)", Cnj [Neg (Prop 1), Prop 2])
  , ("*(-1 -2)", Cnj [Neg (Prop 1), Neg (Prop 2)])
  , ("*(1 -2 -3 4 5 -6 7 -8 9)", Cnj [Prop 1, Neg (Prop 2), Neg (Prop 3), Prop 4, Prop 5, Neg (Prop 6), Prop 7, Neg (Prop 8), Prop 9])
  , ("+(1)", Dsj [Prop 1])
  , ("+(-1)", Dsj [Neg (Prop 1)])
  , ("+(1 2)", Dsj [Prop 1, Prop 2])
  , ("+(1 -2)", Dsj [Prop 1, Neg (Prop 2)])
  , ("+(-1 2)", Dsj [Neg (Prop 1), Prop 2])
  , ("+(-1 -2)", Dsj [Neg (Prop 1), Neg (Prop 2)])
  , ("+(1 -2 -3 4 5 -6 7 -8 9)", Dsj [Prop 1, Neg (Prop 2), Neg (Prop 3), Prop 4, Prop 5, Neg (Prop 6), Prop 7, Neg (Prop 8), Prop 9])
  , ("(1 ==> 2)", Impl (Prop 1) (Prop 2))
  , ("(1 ==> -2)", Impl (Prop 1) (Neg (Prop 2)))
  , ("(-1 ==> 2)", Impl (Neg (Prop 1)) (Prop 2))
  , ("(-1 ==> -2)", Impl (Neg (Prop 1)) (Neg (Prop 2)))
  , ("(1 <=> 2)", Equiv (Prop 1) (Prop 2))
  , ("(1 <=> -2)", Equiv (Prop 1) (Neg (Prop 2)))
  , ("(-1 <=> 2)", Equiv (Neg (Prop 1)) (Prop 2))
  , ("(-1 <=> -2)", Equiv (Neg (Prop 1)) (Neg (Prop 2)))
  , ("((* (1 2) ==> + (1 -2)) <=> (+ (-1 2) ==> * (-1 -2)))", Equiv (Impl (Cnj [Prop 1, Prop 2]) (Dsj [Prop 1, Neg (Prop 2)])) (Impl (Dsj [Neg (Prop 1), Prop 2]) (Cnj [Neg (Prop 1), Neg (Prop 2)])))
  ]

testParser = all (\(str, expr) -> (head . parse $ str) == expr ) validExpressions
