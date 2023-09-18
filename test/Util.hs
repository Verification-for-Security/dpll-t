module Util
  ( prop
  , cnf
  , comparison
  , linear
  , CNF'
  , cnf'
  , set
  ) where

import AST.Prop (Prop ((:&:), (:|:)))
import qualified AST.Prop as P
import AST.LRA (LRA (..))

import Parser (bool, lra)
import Text.Parsec (parse)
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set

import CNF.Types (CNF, Or, Lit (..))

linear :: String -> Prop (LRA Text)
linear text = case parse lra "" text of
  Left err -> error $ show err
  Right p -> p

comparison :: String -> LRA Text
comparison text = case parse lra "" text of
  Right (P.Lit l) -> l
  Left err -> error $ show err
  _ -> error "Expected only (>=) or (<=)"

-- | Parse as a prop
prop :: String -> Prop Text
prop text = case parse bool "" text of
  Left err -> error $ show err
  Right p -> p

-- | Parse as prop, assuming its in cnf form
cnf :: String -> CNF Text
cnf text = case ands . prop $ text of
  Just p' -> p'
  Nothing -> error "prop was not in cnf"

type CNF' a = Set (Set (Lit a))

set :: Ord a => CNF a -> CNF' a
set phi = Set.fromList $ Set.fromList <$> phi

cnf' :: String -> CNF' Text
cnf' = set . cnf

-- Accumulate all the ands, nesting to ors
ands :: Prop a -> Maybe (CNF a)
ands (p :&: q) = do
  p' <- ands p
  q' <- ands q
  return $ p' <> q'
ands p = return <$> ors p

-- Accumulate all the ors, nesting to literals
ors :: Prop a -> Maybe (Or a)
ors (p :|: q) = do
  p' <- ors p
  q' <- ors q
  return $ p' <> q'
ors p = return <$> lit p

-- Get a literal or fail
lit :: Prop a -> Maybe (Lit a)
lit (P.Neg (P.Lit x)) = return $ Neg x
lit (P.Lit x) = return $ Lit x
lit _ = Nothing
