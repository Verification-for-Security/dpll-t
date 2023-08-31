module Main
  ( main
  ) where

import Rename
import Parser (lra)
import SMT (smt)
import Theory.Simplex

import Text.Parsec
import Control.Monad.State
import System.Exit

main :: IO ()
main = do
  raw <- getLine
  result <- runParserT lra () "" raw

  prop <- case result of
    Right p -> return p
    Left e -> die $ show e

  prop' <- evalStateT (mapM withIDs prop) (0, mempty)
  constraints <- case mapM simplex prop' of
    Just p -> return p
    Nothing -> die "Constraints contained non-linear expression"

  print $ case smt constraints of
    Just _ -> "SAT"
    Nothing -> "UNSAT"
