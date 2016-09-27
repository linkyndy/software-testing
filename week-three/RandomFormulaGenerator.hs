module RandomFormulaGenerator where

import Test.QuickCheck
import Common
import ConjunctiveNormalForm
import LogicConcepts

-- We assign different frequencies in order to control the size of the resulting
-- formula
instance Arbitrary Form where
  arbitrary = frequency
    [ (2, return p)
    , (2, return q)
    , (2, return r)
    , (1, do
        f <- arbitrary
        return (Neg f)
      )
    , (1, do
        f <- arbitrary
        g <- arbitrary
        return (Cnj [f, g])
      )
    , (1, do
        f <- arbitrary
        g <- arbitrary
        return (Dsj [f, g])
      )
    , (1, do
        f <- arbitrary
        g <- arbitrary
        return (Impl f g)
      )
    , (1, do
        f <- arbitrary
        g <- arbitrary
        return (Equiv f g)
      )
    ]

-- Check that the initial and CNF-ed formulas are equivalent
prop_equivalent f = equivalence f (cnf f)

-- Check that the CNF-ed formula doesn't contain any implications or equivs
prop_no_impl_or_equiv f = checkImplAndEquiv . cnf $ f

-- Check that the CNF-ed formula doesn't contain double negations or negations
-- of anything other than a simple symbol
prop_no_neg f = checkNeg . cnf $ f

-- Checked that the CNF-ed formula has all disjunctions distributed over
-- conjunctions
prop_distrib f = checkDistrib . cnf $ f

checkImplAndEquiv :: Form -> Bool
checkImplAndEquiv (Prop _) = True
checkImplAndEquiv (Neg f) = checkImplAndEquiv f
checkImplAndEquiv (Cnj fs) = or (map checkImplAndEquiv fs)
checkImplAndEquiv (Dsj fs) = or (map checkImplAndEquiv fs)
checkImplAndEquiv (Impl _ _) = False
checkImplAndEquiv (Equiv _ _) = False

checkNeg :: Form -> Bool
checkNeg (Prop _) = True
checkNeg (Neg (Prop _)) = True
checkNeg (Neg f) = False
checkNeg (Cnj fs) = or (map checkNeg fs)
checkNeg (Dsj fs) = or (map checkNeg fs)
checkNeg (Impl f g) = checkNeg f && checkNeg g
checkNeg (Equiv f g) = checkNeg f && checkNeg g

checkDistrib :: Form -> Bool
checkDistrib (Prop _) = True
checkDistrib (Neg f) = checkDistrib f
checkDistrib (Cnj fs) = or (map checkDistrib fs)
checkDistrib (Dsj [Cnj [p, q], r]) = False
checkDistrib (Dsj [p, Cnj [q, r]]) = False
checkDistrib (Dsj fs) = or (map checkDistrib fs)
checkDistrib (Impl f g) = checkDistrib f && checkDistrib g
checkDistrib (Equiv f g) = checkDistrib f && checkDistrib g
