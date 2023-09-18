import Test.Hspec
import Test.Hrubric
import Test.HUnit

import Control.Monad
import Control.Monad.Trans.Maybe
import Grade

import qualified CNFSpec
import qualified TseitinSpec
import qualified DPLLSpec
import qualified SimplexSpec
import qualified SMTSpec

-- | The complete test suite
rubric :: Rubric
rubric = do
  criterion "It compiles" (1/10) . passOrFail $ 
    it "..." $ True @?= True
  criterion "CNF" (2/10) CNFSpec.rubric
  criterion "Tseitin" (2/10) TseitinSpec.rubric
  criterion "DPLL" (3/10) DPLLSpec.rubric
  criterion "Simplex" (1/10) SimplexSpec.rubric
  criterion "SMT" (1/10) SMTSpec.rubric

main :: IO ()
main = void . runMaybeT $ do
  result <- MaybeT $ hrubric rubric
  pretty result
  autograde result
