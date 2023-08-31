module AST.LRA
  ( LRA (..)
  , Expr (..)
  ) where

infixl 7 :*:
infixl 6 :+:
infix 5 :<=:
infix 5 :>=:

-- | A constraint is a relation that returns a boolean.
--
-- Some notes about constraints:
-- - Simplex only works with non-strict comparisons.
--   This rules out (!=), (>) and (<).
--
-- - To represent (>) and (<), one can negate a Constraint
--   in a proposition. We can then transform the strict
--   inequality into a non-strict version by adding an
--   additional delta. (e.g. x < y becomes x + d <= y)
--
-- - We don't add (==) here directly, as a negation of
--   such a proposition (i.e. (!=)) is not convertible
--   to something simplex can solve.
--
--   Instead, we transform equality to (>=) & (<=) at
--   parse time, as we can negate these propositions
--   separately.
data LRA a
  = Expr a :<=: Expr a
  -- ^ Greater than
  | Expr a :>=: Expr a
  -- ^ Less than
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (LRA a) where
  showsPrec prec constraint = showParen (prec > 5) expr
    where
      expr = showsPrec 6 lhs . showString op . showsPrec 6 rhs
      (lhs, op, rhs) = case constraint of
        lhs' :<=: rhs' -> (lhs', " ≤ ", rhs')
        lhs' :>=: rhs' -> (lhs', " ≥ ", rhs')

-- | An arithemtic expressions.
data Expr a
  = Var a
  -- ^ Variable
  | Constant Rational
  -- ^ Integer
  | Minus (Expr a)
  -- ^ Unary negation
  | Expr a :+: Expr a
  -- ^ Addition
  | Expr a :*: Expr a
  -- ^ Multiplication
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Expr a) where
  showsPrec prec (Var x) = showsPrec prec x
  showsPrec prec (Constant i) = showsPrec prec i
  showsPrec prec (Minus body) = showParen (prec > 9) expr
    where
      expr = showString "-" . showsPrec 9 body
  showsPrec prec (lhs :+: rhs) = showParen (prec > 6) expr
    where
      expr = showsPrec 6 lhs . showString " + " . showsPrec 7 rhs
  showsPrec prec (lhs :*: rhs) = showParen (prec > 7) expr
    where
      expr = showsPrec 7 lhs . showString " * " . showsPrec 8 rhs
