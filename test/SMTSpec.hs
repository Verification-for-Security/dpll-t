module SMTSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import SMT
import Util
import Rename
import Theory.Simplex

import Data.Maybe
import Control.Monad.State

check :: String -> Bool
check text = isJust $ do
  let constraints = linear text
  constraints' <- evalStateT (mapM withIDs constraints) (0, mempty)
  linearConstraints <- mapM simplex constraints'
  smt linearConstraints

rubric :: Rubric
rubric = do
  criterion "dpllT" 1 . passOrFail $ do
    it "solves satisfiable constrants without blocking" $ do
      check "x>=1 & y>=1" @?= True
      check "(x>=1 | y>=1)" @?= True
      check "x=1 | y=2" @?= True
      check "x=1 & y=2 & z=3 | w=4" @?= True
      check "(x>=1 & y<=2) | (z<=3 & w>=4)" @?= True
      check "(x+y>=1 & z+w<=2) | (a+(-b)>=3 & c+d<=4)" @?= True
    it "solves satisfiable constrants with blocking" $ do
      check "x+(-y)=0 & (x+(-y)>=1 | y=0)" @?= True
      check "x+(-y)=0 & (y=0 | x+(-y)>=1)" @?= True
      check "(x>=1 & y<=1) | (x<=1 & y>=1)" @?= True
      check "(x>=1 & y<=1) & !(x<=1 & y>=1)" @?= True
      check "(x=1 | y=1) & !(x=1 & y=1)" @?= True
    it "handles implications correctly" $ do
      check "(x>=1) → y>=1" @?= True
      check "(x<=0) → (y<=0 & x>=0 & y>=1)" @?= True
    it "recognizes unsatisfiable constrants" $ do
      check "(x>=1 & x<=0)" @?= False
      check "(x>=3 & (x<=0 | x<=1))" @?= False
      check "(x>=1 & y<=0) & (y>=1 & x<=0)" @?= False
    it "solves complex constaints (may take a bit of time)" $ do
      check "x+(-z)=0 ∧ ((y+(-z)=0 ∧ x+(-z)<= (-1)) ∨ ¬(x+(-z)=0))" @?= False
      check "(x+y=1 | z+w=2) & !(x+y=2 & z+w=1)" @?= True
      check "((x = 0) ∨ ¬(x = 1) ∨ z=1) ∧ ((x = 0) ∨ (x = 1) ∨ (a=1)) ∧ (¬(x = 0) ∨ (x = 1) ∨ (a=1)) ∧ (¬(a=1) ∨ (y = 1)) ∧ (¬(z=1) ∨ (x + y >= 4)) ∧ (y <= (-1)) ∧ ((a=1) ∨ (x+(-y)=4)) ∧ ((y = 2) ∨ ¬(z=1)) ∧ (x >= 0)" @?= False
