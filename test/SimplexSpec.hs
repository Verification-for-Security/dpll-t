module SimplexSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Theory.Simplex

import Rename
import Util

import qualified Linear.Simplex.Types as Simplex

rubric :: Rubric 
rubric = do
  let simplex' = simplex . (\(_, x, _) -> x) . rename . comparison

  criterion "simplex" 1 . passOrFail $ do
    it "parses linear expressions" $ do
      simplex' "x <= 0" @?= Just (Simplex.LEQ [(0, 1)] 0)
      simplex' "y + -x >= -25" @?= Just (Simplex.GEQ [(0, 1), (1, -1)] (-25))
      simplex' "8 * z + -4 * y + -x <= 14" @?= Just (Simplex.LEQ [(0, 8), (1, -4), (2, -1)] 14)
      simplex' "-1 * a + -b + c + -(-2890) * d >= 234" @?= Just (Simplex.GEQ [(0, -1), (1, -1), (2, 1), (3, 2890)] 234)
    it "rejects non-linear expressions" $ do
      simplex' "1 >= 0" @?= Nothing
      simplex' "1 >= x" @?= Nothing
      simplex' "9 * 10 * x <= 10" @?= Nothing
      simplex' "x * y <= 4" @?= Nothing
