module TseitinSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import CNF (Lit (..))
import qualified CNF

import Util
import Rename (ID (..))

rubric :: Rubric
rubric = do
  let equisat = fst . CNF.equisat . prop
  let lit = Lit . ID
  let neg = Neg . ID

  criterion "tseitin" 1 . passOrFail $ do
    it "has correct lit case" $ do
      equisat "x" @?= [[lit 0]]
    it "has correct neg case" $ do
      equisat "!x" @?= [[neg 1, neg 0], [lit 0, lit 1], [lit 1]]
    it "has correct conjunct case" $ do
      equisat "x & y" @?= [[neg 2, lit 0], [neg 2, lit 1], [neg 0, neg 1, lit 2], [lit 2]]
    it "has correct disjunct case" $ do
      equisat "x | y" @?= [[neg 2, lit 0, lit 1], [neg 0, lit 2], [neg 1, lit 2], [lit 2]]
    it "recurses correctly" $ do
      equisat "(a & b) | (c & d) | (e & f)" @?= [[neg 7, lit 0], [neg 7, lit 1], [neg 0, neg 1, lit 7], [neg 9, lit 2], [neg 9, lit 3], [neg 2, neg 3, lit 9], [neg 10, lit 4], [neg 10, lit 5], [neg 4, neg 5, lit 10], [neg 8, lit 9, lit 10], [neg 9, lit 8], [neg 10, lit 8], [neg 6, lit 7, lit 8], [neg 7, lit 6], [neg 8, lit 6], [lit 6]]
      equisat "(!a & b) | !(c & !d) | (e & f)" @?= [[neg 8, neg 0], [lit 0, lit 8], [neg 7, lit 8], [neg 7, lit 1], [neg 8, neg 1, lit 7], [neg 12, neg 3], [lit 3, lit 12], [neg 11, lit 2], [neg 11, lit 12], [neg 2, neg 12, lit 11], [neg 10, neg 11], [lit 11, lit 10], [neg 13, lit 4], [neg 13, lit 5], [neg 4, neg 5, lit 13], [neg 9, lit 10, lit 13], [neg 10, lit 9], [neg 13, lit 9], [neg 6, lit 7, lit 9], [neg 7, lit 6], [neg 9, lit 6], [lit 6]]
      equisat "!((a & b) | !((c | d) & (e & f)))" @?= [[neg 8, lit 0], [neg 8, lit 1], [neg 0, neg 1, lit 8], [neg 11, lit 2, lit 3], [neg 2, lit 11], [neg 3, lit 11], [neg 12, lit 4], [neg 12, lit 5], [neg 4, neg 5, lit 12], [neg 10, lit 11], [neg 10, lit 12], [neg 11, neg 12, lit 10], [neg 9, neg 10], [lit 10, lit 9], [neg 7, lit 8, lit 9], [neg 8, lit 7], [neg 9, lit 7], [neg 6, neg 7], [lit 7, lit 6], [lit 6]]
      equisat "!(!((c | d) & (e & f)) | (a & b))" @?= [[neg 10, lit 0, lit 1], [neg 0, lit 10], [neg 1, lit 10], [neg 11, lit 2], [neg 11, lit 3], [neg 2, neg 3, lit 11], [neg 9, lit 10], [neg 9, lit 11], [neg 10, neg 11, lit 9], [neg 8, neg 9], [lit 9, lit 8], [neg 12, lit 4], [neg 12, lit 5], [neg 4, neg 5, lit 12], [neg 7, lit 8, lit 12], [neg 8, lit 7], [neg 12, lit 7], [neg 6, neg 7], [lit 7, lit 6], [lit 6]]

