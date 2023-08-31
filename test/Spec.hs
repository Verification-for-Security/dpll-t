import Test.Hspec
import Test.Hrubric
import Test.HUnit

import System.Environment
import System.Console.ANSI
import Text.Printf

import Control.Monad
import Data.Maybe

import qualified CNFSpec
import qualified TseitinSpec
import qualified DPLLSpec
import qualified SimplexSpec
import qualified SMTSpec

rubric :: Rubric
rubric = do
  criterion "It compiles" (1/10) . passOrFail $ 
    it "..." $ True @?= True
  criterion "CNF" (2/10) CNFSpec.rubric
  criterion "Tseitin" (2/10) TseitinSpec.rubric
  criterion "DPLL" (3/10) DPLLSpec.rubric
  criterion "Simplex" (1/10) SimplexSpec.rubric
  criterion "SMT" (1/10) SMTSpec.rubric

-- Output the weight as grade
output :: Float -> IO ()
output g = do
  let color = if g > 0.55 then Green else Red
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "Your current grade is: ["
  setSGR [SetColor Foreground Vivid color]
  putStr $ printf "%.1f" (g * 10)
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStr "/10.0]\n"
  setSGR [Reset]

  -- Output the weight as a grade between 0 and 1 for codegrade
  codegrade <- lookupEnv "CG_INFO"
  when (isJust codegrade) $ print g

main :: IO ()
main = do
  result <- hrubric rubric
  case result of
    Left p -> putStrLn $ "Error in rubric nesting: '" ++ p ++ "'"
    Right g -> output g
