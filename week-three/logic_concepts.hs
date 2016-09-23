module LogicConcepts where

import Common

-- A contradiction happens when a formula cannot be satisfied at all
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- A tautology happens when a formula is valid whatever values it is fed
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- Two formulas are logically entailed when a valid implication happens between
-- all possible values the two formulas may take
entailment :: Form -> Form -> Bool
entailment f g = all (\v -> evl v (Impl f g)) (allVals (Impl f g))

-- Two formulas are logically equivalent if both entailments between them are
-- valid
equivalence :: Form -> Form -> Bool
equivalence f g = entailment f g && entailment g f

p = Prop 1
q = Prop 2

validContradictions =
  [ Cnj [p, Neg p] -- p ∧ ¬p
  , Cnj [Dsj [p, q], Cnj [Neg p, Neg q]] -- (p ∨ q) ∧ (¬p ∧ ¬q)
  ]
invalidContradictions =
  [ p
  , Cnj [p, q] -- p ∧ q
  , Dsj [p, Neg p] -- p ∨ ¬p
  ]
testContradiction = all (\c -> contradiction c) validContradictions && all (\c -> not (contradiction c)) invalidContradictions

validTautologies =
  [ Dsj [p, Neg p] -- p ∨ ¬p
  , Dsj [Dsj [p, q], Cnj [Neg p, Neg q]] -- (p ∨ q) ∨ (¬p ∧ ¬q)
  ]
invalidTautologies =
  [ p
  , Cnj [p, Neg p] -- p ∧ ¬p
  , Dsj [p, q] -- p ∨ q
  ]
testTautology = all (\c -> tautology c) validTautologies && all (\c -> not (tautology c)) invalidTautologies

validEntailments =
  [ (p, Dsj [p, q]) -- p, p ∨ q
  , ((Impl p q), (Impl p q)) -- p ⟶ q, p ⟶ q
  ]
invalidEntailments =
  [ (p, q)
  , (Dsj [p, q], p) -- p ∨ q, p
  , (Cnj [p, q], Cnj [Neg p, Neg q]) -- p ∧ q, ¬p ∧ ¬q
  ]
testEntailment = all (\c -> entailment' c) validEntailments && all (\c -> not (entailment' c)) invalidEntailments
  where
    entailment' (f,g) = entailment f g

validEquivalences =
  [ (p, p) -- p, p
  , ((Impl p q), (Impl p q)) -- p ⟶ q, p ⟶ q
  , ((Impl p q), (Impl (Neg q) (Neg p))) -- p ⟶ q, ¬q ⟶ ¬p
  ]
invalidEquivalences =
  [ (p, q)
  , ((Impl p q), (Impl (Neg p) (Neg q))) -- p ⟶ q, ¬p ⟶ ¬q
  , ((Impl p q), (Impl q p)) -- p ⟶ q, q ⟶ p
  ]
testEquivalence = all (\c -> equivalence' c) validEquivalences && all (\c -> not (equivalence' c)) invalidEquivalences
  where
    equivalence' (f,g) = equivalence f g
