module Rename
  ( ID (..)
  , Renames

  , lookupName
  , withIDs
  , rename
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State

-- | An identifier for variables for which we can
-- easily generate fresh variables.
newtype ID = ID Integer
  deriving (Eq, Ord, Num)

instance Show ID where
  show (ID idx) = 'x' : show idx

-- | A mapping to go from identifiers back to their
-- original representation.
newtype Renames a = Renames (Map ID a)

lookupName :: Renames a -> ID -> Maybe a
lookupName (Renames m) ident = Map.lookup ident m

-- | This is scaffolding code not needed for your implementation. It adds IDs
-- for literals. Literals with the same name get the same IDs.
addID :: (MonadState (ID, Map a ID) m, Ord a) => a -> m ID
addID e = do
  (ident, m) <- get
  case Map.lookup e m of
    Just ident' -> return ident'
    Nothing -> do 
      let ident' = ident + 1
      let m' = Map.insert e ident m
      put (ident', m')
      return ident

withIDs :: (MonadState (ID, Map a ID) m, Ord a, Traversable f) => f a -> m (f ID)
withIDs = mapM addID

rename :: (Traversable f, Ord a) => f a -> (ID, f ID, Renames a)
rename f = (next, f', Renames $ invert renames)
  where
    (f', (next, renames)) = runState (withIDs f) (0, Map.empty)

invert :: Ord b => Map a b -> Map b a
invert = Map.foldrWithKey (flip Map.insert) Map.empty