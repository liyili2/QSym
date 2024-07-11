module QSym.Logic.Syntax
  where

import Prettyprinter

data Range =
  Range
  { rangeName :: String
  , rangeStart :: SimpleExpr
  , rangeEnd :: SimpleExpr
  }
  deriving (Show, Eq)

data SimpleExpr
  = LocusVar SteppedLocus
  | Var String -- | "Logical" variable
  | Xor SimpleExpr SimpleExpr
  | Add SimpleExpr SimpleExpr
  | Lit Int
  | Hadamard String
  deriving (Show, Eq)

-- | Invariant: Ranges in list should be disjoint
newtype Locus = Locus [Range]
  deriving (Show, Eq)

-- | Keeps track of the time step
data Stepped a = Current a | Step (Stepped a)
  deriving (Show, Eq)

type SteppedLocus = Stepped Locus

newtype Prop a = Prop [Conjunct a]
  deriving (Show, Semigroup, Monoid)

data Conjunct a
  = PointsTo SteppedLocus a
  | Unchanged SteppedLocus -- | x[i,j)' = x[i,j)
  deriving (Show)

data LExpr
  = LApply SteppedLocus (Lambda SimpleExpr) 
  | LIfThenElse SimpleExpr LExpr SimpleExpr
  | LSimpleExpr SimpleExpr
  deriving (Show)

data Lambda a = String :=> a
  deriving (Show)

type LExprProp = Prop LExpr
type LExprConjunct = Conjunct LExpr

instance Pretty Range where
  pretty r
    | rangeEnd r == (rangeStart r `Add` Lit 1) = pretty (rangeName r) <> pretty "[" <> pretty (rangeStart r) <> pretty "]"
    | otherwise =
        pretty (rangeName r) <> pretty "[" <> pretty (rangeStart r) <> pretty "," <+> pretty (rangeEnd r) <> pretty ")"

instance Pretty Locus where
  pretty (Locus xs) =
    separatedBy (pretty "++") $ map pretty xs

instance Pretty a => Pretty (Prop a) where
  pretty (Prop xs) =
    separatedBy (pretty "/\\") $ map pretty xs

instance Pretty a => Pretty (Conjunct a) where
  pretty (PointsTo lhs rhs) =
    pretty lhs <+> pretty "|->" <+> pretty rhs

  pretty (Unchanged (Step l)) =
    pretty (Step l) <+> pretty "=" <+> pretty l

instance Pretty LExpr where
  pretty (LApply lhs e) =
    pretty "apply(" <> pretty e <> pretty "," <+> pretty lhs <> pretty ")"

  pretty (LIfThenElse c t f) =
    pretty "ifThenElse(" <> pretty c <> pretty "," <+> pretty t <> pretty "," <+> pretty f <> pretty ")"

  pretty (LSimpleExpr e) = pretty e

instance Pretty a => Pretty (Stepped a) where
  pretty (Current x) = pretty x
  pretty (Step x) = pretty x <> pretty "'"

instance Pretty SimpleExpr where
  pretty (LocusVar x) = pretty x
  pretty (Var x) = pretty x
  pretty (Lit i) = pretty i
  pretty (Xor a b) = pretty a <+> pretty "+" <+> pretty b
  pretty (Hadamard x) = pretty "H" <> parens (pretty x)

instance Pretty a => Pretty (Lambda a) where
  pretty (x :=> body) = parens $ pretty x <+> pretty "=>" <+> pretty body

unitRange :: String -> SimpleExpr -> Range
unitRange x i = Range x i (Add i (Lit 1))

unitLocus :: String -> SimpleExpr -> Locus
unitLocus x i = Locus [unitRange x i]

separatedBy :: Doc a -> [Doc a] -> Doc a
separatedBy separator xs = concatWith go xs
  where
    go a b = a <+> separator <+> b

class Steppable a where
  step :: a -> a

instance Steppable (Stepped a) where
  step = Step

instance Steppable a => Steppable (Prop a) where
  step (Prop xs) = Prop $ map step xs

instance Steppable a => Steppable (Conjunct a) where
  step (PointsTo lhs rhs) = PointsTo (step lhs) (step rhs)
  step (Unchanged x) = Unchanged (step x)

instance Steppable SimpleExpr where
  step (Xor x y) = Xor (step x) (step y)
  step (Add x y) = Add (step x) (step y)
  step (LocusVar x) = LocusVar $ step x
  step x = x -- NOTE: This might need to be extended if we add more constructors to SimpleExpr

instance Steppable a => Steppable (Lambda a) where
  step (x :=> body) = x :=> step body

instance Steppable LExpr where
  step (LApply x rhs) =
    LApply (step x) (step rhs)

  step (LIfThenElse c t f) =
    LIfThenElse (step c) (step t) (step f)

  step (LSimpleExpr e) = LSimpleExpr $ step e

testProp :: LExprProp
testProp =
  Prop
    [ -- Step 0
      PointsTo (Current (unitLocus "q" (Lit 0))) (LSimpleExpr (Var "a"))
    , PointsTo (Current (unitLocus "q" (Lit 1))) (LSimpleExpr (Var "b"))

      -- Step 1
    , PointsTo (Step (Current (unitLocus "q" (Lit 0)))) (LApply (Current (unitLocus "q" (Lit 0))) ("x" :=> Hadamard "x"))
    , Unchanged (Step (Current (unitLocus "q" (Lit 1))))

      -- Step 2
    , Unchanged (Step (Step (Current (unitLocus "q" (Lit 0)))))
    , PointsTo (Step (Step (Current (unitLocus "q" (Lit 1)))))
        $ LIfThenElse (LocusVar (Step (Current (unitLocus "q" (Lit 0)))))
            (LApply (Step (Current (unitLocus "q" (Lit 1)))) ("x" :=> Xor (Var "x") (Lit 1)))
            (LocusVar (Step (Current (unitLocus "q" (Lit 1)))))
    ]
