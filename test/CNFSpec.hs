module CNFSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Util (prop, cnf)

import qualified CNF.Transform as CNF

rubric :: Rubric
rubric = do
  criterion "Prop" (1/10) . distribute $ do
    dcriterion "-->" . passOrFail $ do
      it "is defined in terms of Neg and :|:" $ do
        prop "x --> y" @?= prop "!x | y"
        prop "(x & y) --> (y & z)" @?= prop "!(x & y) | (y & z)"

    dcriterion "<->" . passOrFail $ do
      it "is defined in terms of --> and :&:" $ do
        prop "x <-> y" @?= prop "(!x | y) & (!y | x)"

  criterion "Transform" (9/10) $ do
    criterion "distribute" (1/2) . passOrFail $ do
      it "correctly computes the base case (no conjuncts)" $ do
        CNF.distribute (cnf "x") (cnf "!y") @?= cnf "x | !y"
        let lhs = cnf "a | b | c"
        let rhs = cnf "d | e | f"
        CNF.distribute lhs rhs  @?= cnf "a | b | c | d | e | f"

      it "correctly distributes singles" $ do
        let a  = cnf "a"
        let bcd = cnf "b & c & d"
        CNF.distribute a bcd @?= cnf "(a | b) & (a | c) & (a | d)"
        CNF.distribute bcd a @?= cnf "(b | a) & (c | a) & (d | a)"

      it "correctly recursively distributes" $ do
        let lhs0 = cnf "a & b"
        let rhs0 = cnf "c & d"
        let answer0 = cnf "(a | c) & (a | d) & (b | c) & (b | d)"
        CNF.distribute lhs0 rhs0 @?= answer0

        let lhs1 = cnf "a & b & c"
        let rhs1 = cnf "d & (e | f)"
        let answer1 = cnf "(a | d) & (a | e | f) & (b | d) & (b | e | f) & (c | d) & (c | e | f)"
        CNF.distribute lhs1 rhs1 @?= answer1

    criterion "cnf" (1/2) . passOrFail $ do
      let transform = CNF.cnf . prop

      it "keeps literals (and their negation)" $ do
        transform "x" @?= cnf "x"
        transform "!x" @?= cnf "!x"

      it "removes double negatives" $ do
        transform "!(!x)" @?= cnf "x"
        transform "!(!(!x))" @?= cnf "!x"

      it "distributes disjuncts (and leaves conjuncts)" $ do
        let conjuncts = "(a & b) & (c & d & e & f)"
        transform conjuncts @?= cnf conjuncts

        let disjuncts = "(a | b) & (c | d | e | f)"
        transform disjuncts @?= cnf disjuncts

        let both = "(a & b) | (c & d)"
        let result = "(a | c) & (a | d) & (b | c) & (b | d)"
        transform both @?= cnf result

      it "applies De-Morgans law" $ do
        transform "!(x & y)" @?= cnf "!x | !y"
        transform "!(x | y)" @?= cnf "!x & !y"

      it "recursively applies all cases" $ do
        let result0 = "p & p & q"
        transform "!(p --> !(p & q))" @?= cnf result0

        let result1 = "(a | b) & (!c | d)"
        transform "(a | !(!b)) & (c -> d)" @?= cnf result1

        let result2 = "(!a | !b | c) & (b | a) & (!c | a)"
        transform "a <-> (b --> c)" @?= cnf result2

        let result3 = "(a | !c | !d | e) & (a | !c | !d | !f) & (b | !c | !d | e) & (b | !c | !d | !f)"
        transform "(!(!a) & b) | !(c & d) | (e & !(!(!f)))" @?= cnf result3

        let result4 = "(a | c | e) & (a | c | f) & (a | d | e) & (a | d | f) & (b | c | e) & (b | c | f) & (b | d | e) & (b | d | f)"
        transform "(a & b) | (c & d) | (e & f)" @?= cnf result4

        let result5 = "(!a | !b | !c | d) & (!a | !b | !d | c) & (b | a | !c |d) & (b | a | !d | c) & (c | d | a | !b) & (c | d | a | !a) & (c | d | b | !b) & (c | d | b | !a) & (c | !c | a | !b) & (c | !c | a | !a) & (c | !c | b | !b) & (c | ! c | b | !a) & (!d | d | a | !b) & (!d | d | a | !a) & (!d | d | b | !b) & (!d | d | b | !a) & (!d | !c | a | !b) & (!d | !c | a | !a) & (!d | !c | b | !b) & (!d | !c | b | !a)"
        transform "!(a <-> !b) <-> (c <-> d)" @?= cnf result5
