module ConjunctiveNormalForm where

import Common

-- Checks whether a given formula contains nested conjunctions
noNestedCnjs :: Form -> Bool
noNestedCnjs (Prop _) = True
noNestedCnjs (Neg (Prop _)) = True
noNestedCnjs (Dsj fs) = all noNestedCnjs fs
noNestedCnjs _ = False

-- Distributes disjunction over conjunction, or, makes a list of ANDs of ORs.
-- NOTE: A drawback of the current implementation is that conjunctions "hidden"
--       is disjunctions having more than 2 elements will not be treated.
-- NOTE: There is no pattern match for implications/equivs because they are
--       already removed by this point (it's a precondition)
distribute :: Form -> Form
distribute (Prop x) = Prop x
distribute (Neg f) = Neg (distribute f)
distribute (Cnj fs) = Cnj (map distribute fs)
distribute (Dsj [Cnj [p, q], r]) = Cnj [distribute (Dsj [p, r]), distribute (Dsj [q, r])]
distribute (Dsj [p, Cnj [q, r]]) = Cnj [distribute (Dsj [p, q]), distribute (Dsj [p, r])]
distribute (Dsj fs) = if (noNestedCnjs (Dsj fs)) then Dsj (map distribute fs)
                                      else distribute(Dsj (map distribute fs))
distribute f = f

cnf :: Form -> Form
cnf = distribute . nnf . arrowfree
