module SMT
  ( smt
  ) where

import Prelude hiding (negate)

import Rename
import CNF
import AST.Prop (Prop)
import Theory.Class

import Control.Applicative
import Data.Maybe (mapMaybe)

-- | Rename a solution so it becomes a set of constraints for a Theory.
--
-- Notice that Solution and Constraints are essentially the same type
-- under a different name.
asConstraints :: Renames a -> Solution ID -> Constraints a
asConstraints = undefined

-- | The DPLL(T) algorithm.
--
-- Extends the normal DPLL algorithm to work over theorems.
-- This implementation solves only over a single theorem at a time.
--
-- The algorithm works as follows:
-- 1.  Find a solution over a CNF ID, negligent of what the IDs represent.
-- 2.  Get theory constraints by replacing the IDs of this SAT solution to
--     their corresponding theory symbols.
-- 3.  Try to solve this new set of constraints.
-- a.  On succes. We return the Model
-- b.  On failure. We strenghthen our CNF with an element wise negation of the
--     original solution over IDs, then recursively call dpllT.
--
-- Notice how in step 3b we essentially tell the SAT solver to not pick this
-- specific solution via the additional constraints. Otherwise the SAT solver 
-- would infinitely pick the same solution! 
--
-- Hint: Check the 'Theory' typeclass.
dpllT :: Theory a => CNF ID -> Renames a -> Maybe (Model a)
dpllT = undefined

-- | Run the SMT solver on a proposition over some solvable
-- theory.
smt :: (Theory a, Ord a) => Prop a -> Maybe (Model a)
smt = uncurry dpllT . equisat
