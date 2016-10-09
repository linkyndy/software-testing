module MinimalSudoku where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Sudoku

-- Checks whether given problem has a unique solution
uniqueProblem :: Node -> Bool
uniqueProblem = uniqueSol

-- Checks whether all given problem's subproblems have more than one solution.
-- A subproblem is computed by removing a hint from the problem
notUniqueSubproblems :: Node -> Bool
notUniqueSubproblems p = all ((>1) . numberOfSolutions . eraseN p) (filledPositions (fst p))
  where
    numberOfSolutions sp = length $ solveNs [sp]

prop_minimalProblem :: Property
prop_minimalProblem = monadicIO $ do
  s <- run $ genRandomSudoku
  p <- run $ genProblem s
  assert (uniqueProblem p && notUniqueSubproblems p)

-- *MinimalSudoku> quickCheckWith stdArgs { maxSuccess = 5 } prop_minimalProblem
-- +++ OK, passed 5 tests.
-- (63.00 secs, 34,144,726,016 bytes)

-- NOTE: This doesn't currently work due to how the Sudoku solver is defined
-- instance Arbitrary Node where
--   arbitrary = do
--     sudoku <- genRandomSudoku
--     node <- genProblem sudoku
--     return (node)
--
-- prop_minimalProblem :: Node -> Property
