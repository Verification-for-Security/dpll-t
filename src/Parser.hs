module Parser
  ( bool
  , lra
  ) where

import AST.Prop
import AST.LRA

import Parser.LRA
import Parser.Prop
import Parser.Lexeme

import Data.Text (Text)

import Text.Parsec hiding (parse)

program :: Stream s m Char => ParsecT s u m (Prop a) -> ParsecT s u m (Prop a)
program atom = do
  spaces
  p <- prop atom
  eof
  return p

bool :: Stream s m Char => ParsecT s u m (Prop Text)
bool = program $ fmap Lit identifier

lra :: Stream s m Char => ParsecT s u m (Prop (LRA Text))
lra = program constraint
