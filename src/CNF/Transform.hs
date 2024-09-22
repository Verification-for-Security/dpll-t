module CNF.Transform
  ( distribute
  , cnf
  ) where

import AST.Prop
import CNF.Types (CNF)
import qualified CNF.Types as CNF

-- | This function implements the distribution 
-- of disjunction over conjunction.
--
-- The function takes as arguments two 
-- propositions p and q, representing formula p | q, 
-- and exhaustively applies the distribution operation.
--
-- An example of a distribution:
--
-- $ distribute (a & b & c) (d & (e | f))
-- > (a | d) & (a | e | f) & (b | d) & (b | e | f) & (c | d) & (c | e | f)
--
-- Note that this solution will be in the rigid CNF format, which
-- means that the actual result will look more like:
--
-- $ distribute [[a], [b], [c]] [[d], [e, f]]
-- > [ [a, d], [a, e, f], [b, d], [b, e, f], [c, d], [c, e, f] ]
--
-- Hint: You may want to define a helper function that distributes a single Or
-- over the entire second argument.
--
-- Though won't grade on it, we recommend you to implement this function
-- without pattern matching on the lists. Haskell already provides all the 
-- necessary list operations to do so!
distribute :: CNF a -> CNF a -> CNF a
distribute = undefined

-- | Converts a proposition into a rigid CNF.
--
-- Cases of interest your code has to handle for a correct CNF implemention:
-- - Literals and their Negation
-- - Connectives
-- - Double negative
-- - Negatives over connectives
--
-- Note that as the names Neg and Lit are both defined in Prop and CNF, you
-- need to use CNF.Lit or CNF.Neg to differentiate them from those defined in
-- Prop.
--
-- Hint: [[CNF.Neg p]] = !p
--
-- Why do we have a separate type for CNF instead of Prop?
-- - CNF is a rigid form of Prop; it can only ever represent formulas in CNF.
--   Prop may represent formulas in CNF, but also  other formulas. Using the
--   rigid form allows us to avoid runtime checks to see whether a Prop is in
--   CNF as needed for DPLL.
cnf :: Prop a -> CNF a
cnf = undefined
