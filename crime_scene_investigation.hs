module CrimeSceneInvestigation where

import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = accuses Matthew x /= accuses Peter x
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers x = filter (\b -> accuses b x) boys

guilty, honest :: [Boy]
guilty = filter (\b -> length (accusers b) == 3) boys
honest = boys \\ guilty
