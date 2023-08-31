module Parser.Prop
  ( prop
  ) where

import AST.Prop
import Parser.Lexeme

import Text.Parsec
import Text.Parsec.Expr

-- | An operator table for propositional logic.
operatorTable :: Stream s m Char => OperatorTable s u m (Prop a)
operatorTable = 
  [ [Prefix negation]
  , [Infix conjunction AssocRight]
  , [Infix disjunction AssocRight]
  , [Infix implication AssocRight]
  , [Infix biImplication AssocNone]
  ]
  where
    negation = operator Neg ["~", "!", "¬"]
    conjunction = operator (:&:) ["&", "∧"]
    disjunction = operator (:|:) ["|", "∨"]
    implication = operator (-->) ["-->", "->", "→"]
    biImplication = operator (<->) ["<->", "↔"]

-- | A expression is either a parenthesized proposition or an atom.
term :: Stream s m Char => ParsecT s u m (Prop a) -> ParsecT s u m (Prop a)
term atom = parens (prop atom) <|> atom

-- | A full proposition. The passed parsed is used to parse atoms.
prop :: Stream s m Char => ParsecT s u m (Prop a) -> ParsecT s u m (Prop a)
prop atom = buildExpressionParser operatorTable $ term atom
