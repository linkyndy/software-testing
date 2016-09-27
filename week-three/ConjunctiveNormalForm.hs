module ConjunctiveNormalForm where

import Common

-- Distributes disjunction over conjunction, or, makes a list of ANDs of ORs.
-- NOTE: A drawback of the current implementation is that conjunctions "hidden"
--       is disjunctions having more than 2 elements will not be treated.
distribute :: Form -> Form
distribute (Cnj fs) = Cnj (map distribute fs)
distribute (Dsj [Cnj [p, q], r]) = Cnj [distribute (Dsj [p, r]), distribute (Dsj [q, r])]
distribute (Dsj [p, Cnj [q, r]]) = Cnj [distribute (Dsj [p, q]), distribute (Dsj [p, r])]
distribute f = f

cnf :: Form -> Form
cnf = distribute . nnf . arrowfree
