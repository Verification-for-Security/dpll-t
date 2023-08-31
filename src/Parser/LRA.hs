module Parser.LRA
  ( constraint
  ) where

import AST.Prop
import AST.LRA
import Parser.Lexeme

import Text.Parsec
import Text.Parsec.Expr

import Data.Text (Text)

-- | An operator table for linear rational arithmetic expressions.
operatorTable :: Stream s m Char => OperatorTable s u m (Expr a)
operatorTable = 
  [ [Prefix minus]
  , [Infix multiplication AssocLeft]
  , [Infix addition AssocLeft]
  ]
  where
    minus = operator Minus ["-"]
    multiplication = operator (:*:) ["*"]
    addition = operator (:+:) ["+"]

atom :: Stream s m Char => ParsecT s u m (Expr Text)
atom = fmap Var identifier <|> fmap (Constant . fromIntegral) natural
-- TODO: The Constant parse here should ideally be a rational instead of natural

term :: Stream s m Char => ParsecT s u m (Expr Text)
term = parens expr <|> atom

expr :: Stream s m Char => ParsecT s u m (Expr Text)
expr = buildExpressionParser operatorTable term

comparison :: Stream s m Char => ParsecT s u m (Expr a -> Expr a -> Prop (LRA a))
comparison = choice [le, ne, ge, lt, eq, gt]
  where
    le = operator (lit (:<=:)) ["<=", "≤"]
    ge = operator (lit (:>=:)) [">=", "≥"]

    lt = operator (neg (:>=:)) ["<"]
    gt = operator (neg (:<=:)) [">"]

    lit op lhs rhs = Lit $ lhs `op` rhs
    neg op lhs rhs = Neg . Lit $ lhs `op` rhs

    eq = operator (===) ["==", "="]
    ne = operator (!==) ["!=", "/=", "≠"]
    
    lhs === rhs = Lit (lhs :>=: rhs) :&: Lit (lhs :<=: rhs)
    lhs !== rhs = Neg $ lhs === rhs

constraint :: Stream s m Char => ParsecT s u m (Prop (LRA Text))
constraint = do
  lhs <- expr
  op <- comparison
  rhs <- expr
  return $ lhs `op` rhs
