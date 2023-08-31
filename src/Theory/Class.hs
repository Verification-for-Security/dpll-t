module Theory.Class
  ( Theory (..)
  , Constraints
  ) where

import CNF

class Theory a where
  type Model a

  solve :: Constraints a -> Maybe (Model a)

type Constraints a = [Lit a]
