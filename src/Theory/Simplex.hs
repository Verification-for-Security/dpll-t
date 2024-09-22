{-# OPTIONS_GHC -Wno-orphans #-}

module Theory.Simplex
  ( simplex
  ) where

import CNF hiding (negate)
import Rename
import AST.LRA
import Theory.Class

import Control.Applicative (empty)
import Data.Ratio

import qualified Linear.Simplex.Types as Simplex
import qualified Linear.Simplex.Simplex as Simplex

deriving instance Ord Simplex.PolyConstraint

instance Theory Simplex.PolyConstraint where
  type Model Simplex.PolyConstraint = ()

  solve constraints = do
    let deltaPos = Simplex.GEQ [(delta, 1)] (1 % 0xffffffff)
    let constraints' = deltaPos : fmap unlit constraints
    _ <- Simplex.findFeasibleSolution constraints'
    return ()

-- | The unique identifier used to write strict inequalities (>) and (<).
delta :: Integer
delta = 0xffffffff

-- | Transform a constraint into one without possible negation.
unlit :: Lit Simplex.PolyConstraint -> Simplex.PolyConstraint
unlit (Lit c) = c
unlit (Neg (Simplex.LEQ lhs rhs)) = Simplex.GEQ ((delta, 1):lhs) rhs
unlit (Neg (Simplex.GEQ lhs rhs)) = Simplex.LEQ ((delta, -1):lhs) rhs
unlit _ = error "Constraints should not contain equalities"

-- | Place the expression into its rigid linear form.
-- Such an expression is of the form:
-- a1 * x1 + a2 * x2 .. an * xn = b
--
-- You are *not* expected to transform the expression if it is not in this 
-- shape. Simply returning empty is sufficient in such a case!
--
-- You may want to look up the Simplex solver library to find out exactly.
-- How it represents this linear form.
--
-- Although not required, we advice you to implement and use the helper
-- functions below.
simplex :: LRA ID -> Maybe Simplex.PolyConstraint
simplex = undefined

-- | Place the expression into its rigid linear form.
-- Such an expression is of the form:
-- a1 * x1 + a2 * x2 .. an * xn
--
-- You are *not* expected to distribute the expression if it is not in this 
-- shape. Simply returning empty is sufficient in such a case!
linear :: Expr ID -> Maybe Simplex.VarConstMap
linear = undefined

-- | Convert the expression into just the Identifier if applicable.
--
-- Return 'empty' if the expression did not respresent a variable.
variable :: Expr ID -> Maybe Integer
variable = undefined

-- | Convert the expression to a Rational if applicable.
--
-- Return 'empty' if the expression did not respresent a constant.
constant :: Expr ID -> Maybe Rational
constant = undefined
