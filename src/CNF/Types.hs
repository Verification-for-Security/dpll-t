module CNF.Types
  ( CNF
  , Or
  , Lit (..)
  , negate
  ) where

import Prelude hiding (negate)

-- | All elements in the CNF type are Conjunct
-- to each other.
--
-- e.g. [p, q, r] == p & q & r
--      []        == True
type CNF a = [Or a]

-- | All elements in the CNF type are Disjunct
-- to each other. 
--
-- e.g. [p, q, r] == p | q | r
--      []        == False
type Or a = [Lit a]

-- | In a CNF, we either have a literal
-- or a negation of it. (No double negation!)
data Lit a = Lit a | Neg a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Negate a literal.
negate :: Lit a -> Lit a
negate (Lit x) = Neg x
negate (Neg x) = Lit x
