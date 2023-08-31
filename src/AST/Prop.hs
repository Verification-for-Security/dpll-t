module AST.Prop
  ( Prop (..)
  , (-->)
  , (<->)
  , assocr
  ) where

-- Define operator precedence for binary
-- operators. :&: binds before :|:, similar
-- to how * binds before + in arithmetic.
infixr 3 :&:
infixr 2 :|:
infixr 1 -->
infix 0 <->

-- | Propositional logic format
--
-- We define the data constructors of the minimal
-- language where (and = :&:, or = :|:)
data Prop a
  = Lit a
  -- ^ Literal
  | Neg (Prop a)
  -- ^ Negation
  | Prop a :&: Prop a
  -- ^ Logical And
  | Prop a :|: Prop a
  -- ^ Logical Or
  deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (Prop a) where
  showsPrec prec (Neg p :|: q) = showParen (prec > 1) expr
    where
      expr = showsPrec 2 p . showString " → " . showsPrec 1 q
  showsPrec prec (p :|: q) = showParen (prec > 2) expr
    where
      expr = showsPrec 3 p . showString " ∨ " . showsPrec 2 q
  showsPrec prec (p :&: q) = showParen (prec > 3) expr
    where
      expr = showsPrec 4 p . showString " ∧ " . showsPrec 3 q
  showsPrec prec (Neg p) = showParen (prec > 9) expr
    where
      expr = showString "¬" . showsPrec 9 p 
  showsPrec prec (Lit x) = showsPrec prec x


-- | Right associate a propositional expression.
-- Since :&: and :|: are commutative, meaning
-- is preserved. This is usefull for printing
-- to improve readability by reducing parentheses.
assocr :: Prop a -> Prop a
assocr ((p :&: q) :&: r) = assocr $ p :&: q :&: r
assocr ((p :|: q) :|: r) = assocr $ p :|: q :|: r
assocr (p :&: q) = assocr p :&: assocr q
assocr (p :|: q) = assocr p :|: assocr q
assocr (Neg p) = Neg $ assocr p
assocr p = p

-- | Implication
(-->) :: Prop a -> Prop a -> Prop a
(-->) = undefined

-- | Bi-Implication
(<->) :: Prop a -> Prop a -> Prop a
(<->) = undefined
