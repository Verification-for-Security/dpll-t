module DPLLSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import CNF
import CNF.DPLL
import Util
import Rename

import Data.Maybe (fromJust, isJust)
import Data.Functor.Identity (runIdentity)
import Control.Monad (foldM)
import Control.Monad.Writer
import Control.Monad.Trans.Maybe (runMaybeT)

sat :: String -> Bool
sat text = isJust $ do
  let phi = fst . equisat . prop $ text
  model <- satisfiable phi 
  return $ check model phi

check :: Eq a => Solution a -> CNF a -> Bool
check model = all checkOr
  where
    checkOr = any (`elem` model)

rubric :: Rubric
rubric = distribute $ do
  let lit = Lit . ID
  let neg = Neg . ID

  let runSolver = runIdentity . runMaybeT . runWriterT

  dcriterion "resolve" . passOrFail $ do
    let resolve' l phi = fst . fromJust . runSolver $ resolve phi l

    it "removes Or if it contained the literal" $ do
      resolve' (lit 0) [] @?= []
      resolve' (lit 0) [[lit 0]] @?= []
      resolve' (lit 0) [[], [lit 1, lit 0, lit 2], [lit 2]] @?= [[], [lit 2]]

    it "removes literal from Or if negation was contained" $ do
      resolve' (lit 0) [[neg 0]] @?= [[]] 
      resolve' (neg 2) [[], [lit 1, lit 0, lit 2], [lit 2]] @?= [[], [lit 1, lit 0], []]

    it "does both operations when cases are mixed" $ do
      resolve' (neg 1) [[neg 1, lit 1], [lit 0, lit 1, lit 2], [neg 1]] @?= [[lit 0, lit 2]]
      resolve' (lit 0) [[neg 1, neg 0, lit 2], [lit 0, lit 1, lit 2]] @?= [[neg 1, lit 2]]

    it "adds literals to the model when it resolves them" $ do
      let model' ls phi = snd . fromJust . runSolver $ foldM resolve phi ls

      let lits0 = [lit 0, neg 1, lit 3] 
      model' lits0 [] @?= lits0
      let lits1 = [lit 0, lit 3, lit 2] 
      model' lits1 [[lit 0, neg 2], [neg 3, lit 1]] @?= lits1

  dcriterion "bcp" . passOrFail $ do
    let bcp' = fst . fromJust . runSolver . bcp

    it "resolves occurences of single literals" $ do
      bcp' [[neg 0], [lit 0, lit 1, lit 2]] @?= [[lit 1, lit 2]]
      bcp' [[lit 0, lit 1, lit 2], [lit 0]] @?= []
      bcp' [[neg 0, lit 1, lit 2, neg 3], [lit 0], [lit 3]] @?= [[lit 1, lit 2]]
      bcp' [[lit 3], [neg 0, lit 1, lit 2, neg 3], [lit 0]] @?= [[lit 1, lit 2]]
      bcp' [[lit 0], [neg 0]] @?= [[]]
      bcp' [[neg 0], [lit 0]] @?= [[]]

    it "recursively solves new single literals" $ do
      bcp' [[neg 0, neg 1, lit 2, neg 3], [neg 0, lit 1], [neg 0, neg 1, neg 2], [lit 0]] @?= []
      bcp' [[neg 0, lit 1], [lit 0], [lit 2, neg 1]] @?= []

  dcriterion "ple" . passOrFail $ do
    let ple' = fst . fromJust . runSolver . ple

    it "resolves occurences of pure literals" $ do
      ple' [[neg 0], [lit 0, lit 1, lit 2]] @?= []
      ple' [[neg 0], [lit 0], [lit 2]] @?= [[neg 0], [lit 0]]
      ple' [[neg 0], [lit 0], [neg 2]] @?= [[neg 0], [lit 0]]
      ple' [[neg 0], [lit 0], [lit 2, lit 0], [lit 1]] @?= [[neg 0], [lit 0]]
    it "recursively resolves new pure literals" $ do
      ple' [[lit 0, lit 1], [lit 0, neg 1, lit 2], [neg 2]] @?= []

  dcriterion "branch" . passOrFail $ do
    it "returns a satisfying model, if any exists" $ do
      sat "a | b" @?= True
      sat "a & !a" @?= False
      sat "b & (!b | a) & !a" @?= False
      sat "b & (b | a) & !a" @?= True
      sat "a & b & c & d & e & f & g & (!a | !b)" @?= False
      sat "a & !b & c & d & e & f & g & (!a | !b)" @?= True
      sat "(a | b | c) & (!a | !b | !c)" @?= True
      sat "(a | b | c) & (!a & (!b | !(a & c)) | !c)" @?= True
      sat "(a | b | c | d | e) & !a & !b & !c & !d & !e" @?= False
      sat "(x | y | z) & (x | y | !z) & (x | !y | z) & (x | !y | !z) & (!x | y | z) & (!x | y | !z) & (!x | !y | z) & (!x | !y | !z)" @?= False
      sat "(¬p ∨ q ∨ r ) ∧ (¬q ∨ r ) ∧ (¬q ∨ ¬r ) ∧ (p ∨ ¬q ∨ ¬r )" @?= True
      sat "(p ∨ ¬q)∧ (q ∨ ¬r) ∧ (r ∨ ¬p)" @?= True
      sat "(p ∧ ¬p)" @?= False
      sat "(p → (q → r )) ∧ ¬((p ∧ q) → r )" @?= False
      sat "(!b|a|!c)&(b|a|!c)&(!b|!a|!c)&(b)&(c)" @?= False
      sat " (x ∨ y) ∧ (¬x ∨ y) ∧ (¬y)" @?= False
      sat "(a ∨ b ∨ ¬c) ∧ (a ∨ c) ∧ (a ∨ ¬b) ∧ (¬a)" @?= False
      sat "x ∨ y → ¬x ∧ z" @?= True
      sat "(𝑥∨𝑦∨𝑧)∧(𝑥∨𝑦∨¬𝑧)∧(𝑥∨¬𝑦∨𝑧)∧(𝑥∨¬𝑦∨¬𝑧)∧(¬𝑥∨𝑦∨𝑧)∧(¬𝑥∨𝑦∨¬𝑧)∧(¬𝑥∨¬𝑦∨𝑧)∧(¬𝑥∨¬𝑦∨¬𝑧)" @?= False
      sat "(x ∨ y)&(¬y∨z∨a)&(¬x∨a)" @?= True
