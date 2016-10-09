module EmptyBlocksSudoku where

-- import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Sudoku

-- Returns a list of all positions, grouped per block
allPositions :: [[(Row, Column)]]
allPositions = [[(r,c) | r <- b1, c <- b2] | b1 <- blocks, b2 <- blocks]

-- Removes values for positions of `nr` random blocks inside the given node
emptySomeBlocks :: Int -> Node -> IO Node
emptySomeBlocks nr n = do
  shuffledAllPositions <- randomize allPositions
  let positionsToBeRemoved = take nr shuffledAllPositions
      newNode = foldl (\n p -> eraseN n p) n (concat positionsToBeRemoved)
  return (newNode)

prop_validProblem :: Property
prop_validProblem = monadicIO $ do
  s <- run $ genRandomSudoku
  s' <- run $ emptySomeBlocks 3 s
  p <- run $ genProblem s'
  let numberOfSolutions = length $ solveNs [p]
  assert (numberOfSolutions > 0)

-- Testing with 3 empty blocks...
-- *EmptyBlocksSudoku> quickCheckWith stdArgs { maxSuccess = 20 } prop_validProblem
-- +++ OK, passed 20 tests.
-- (17.03 secs, 9,175,100,112 bytes)

-- Testing with 4 empty blocks...
-- *EmptyBlocksSudoku> quickCheckWith stdArgs { maxSuccess = 20 } prop_validProblem
-- +++ OK, passed 20 tests.
-- (18.51 secs, 10,727,038,216 bytes)

-- Testing with 5 empty blocks...
-- *EmptyBlocksSudoku> quickCheckWith stdArgs { maxSuccess = 20 } prop_validProblem
-- ^C*** Failed! Exception: 'user interrupt' (after 3 tests):
-- (132.68 secs, 65,789,502,424 bytes)

-- Testing with 6 empty blocks...
-- *EmptyBlocksSudoku> quickCheckWith stdArgs { maxSuccess = 20 } prop_validProblem
-- ^C*** Failed! Exception: 'user interrupt' (after 1 test):
-- (97.60 secs, 49,542,206,104 bytes)

-- Conclusion: increasing the number of empty blocks increases the necessary
-- time to generate a problem.
