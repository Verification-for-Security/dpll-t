module CNF.Tseitin
  ( equisat
  , fresh
  ) where

import Rename
import AST.Prop
import CNF.Transform (cnf)
import CNF.Types (CNF)

import Control.Monad.State
import Control.Monad.Writer

-- | Use this to get a fresh propositional literal.
fresh :: MonadState ID m => m (Prop ID)
fresh = do
  ident <- get
  put $ ident + 1
  return $ Lit ident

-- | Tseitins transformation
--
-- Transforms a propositional formula into an equisatisfiable CNF formula while
-- avoiding the exponential space blow-up that happens with the normal CNF
-- transformation.
--
-- Note: In Tseitin's transformation, we traverse the propositional formula
-- and introduce new names and definitions for the subexpressions. Your task is
-- to implement this transformation here. Please revisit the lecture slides and
-- make sure you thoroughly understand Tseitin's transformation before
-- attempting to implement it.
--
-- What are the Monad typeclasses used in the definition of tseitin?
-- - tseitin uses both the State and Writer interfaces. This means that we can
--   use `put`, `get` and `tell`.
--
-- What do we need these for?
-- - State: You don't need to modify the state (using put/get) directly. The
--   state is used to implement the `fresh` helper function. The `fresh` helper
--   function returns a fresh literal. You can use this to create new names for
--   subformulas.
-- - Writer: The writer is used to *accumulate* the final outcome of the tseitin
--   transformation. That is, whenver you created a definition for a subformula,
--   you can use `tell` to add it to the final CNF. Use your implementation of
--   'cnf' to transform a general proposition formula into a rigid CNF formula. 
--
-- What is the return value of tseitin?
-- - The return value is a (Prop ID), which should always be the propositional
--   variable used to represent the current formula.
-- - We don't introduce new names for literals, so you may directly return a
--   literal when encountered.
--
-- Note: The automatic tests have some requirements on the implementation.
-- - The fresh variable should be on the lhs of the bi-implication
-- - You should first get a fresh variable before recursing
-- - On binary operands, the left hand side should be recursed first
tseitin :: (MonadState ID m, MonadWriter (CNF ID) m) => Prop ID -> m (Prop ID)
tseitin = undefined

-- | Get an equisatisfiable CNF by tseitins transformation. Renames the
-- CNF before running the transformation, such that we can create fresh
-- variables.
equisat :: Ord a => Prop a -> (CNF ID, Renames a)
equisat p = (expr, names)
  where
    expr = evalState (execWriterT tseitin') initial
    tseitin' = tseitin p' >>= tell . cnf
    (initial, p', names) = rename p
