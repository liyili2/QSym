module QSym.Logic.Syntax
  where

import Prettyprinter

data Range =
  Range
  { rangeName :: String
  , rangeStart :: Int
  , rangeEnd :: Int
  }
  deriving (Show)

data SimpleExpr
  = LocusVar SteppedLocus
  | Var String -- | "Logical" variable
  | Xor SimpleExpr SimpleExpr
  | Lit Int
  | Hadamard String
  deriving (Show)

-- | Invariant: Ranges in list should be disjoint
newtype Locus = Locus [Range]
  deriving (Show)

-- | Keeps track of the time step
data Stepped a = Current a | Step (Stepped a)
  deriving (Show)

type SteppedLocus = Stepped Locus

data Prop a = Prop [Conjunct a]
  deriving (Show)

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
    | rangeEnd r == rangeStart r + 1 = pretty (rangeName r) <> pretty "[" <> pretty (rangeStart r) <> pretty "]"
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

unitRange :: String -> Int -> Range
unitRange x i = Range x i (i+1)

unitLocus :: String -> Int -> Locus
unitLocus x i = Locus [unitRange x i]

separatedBy :: Doc a -> [Doc a] -> Doc a
separatedBy separator xs = concatWith go xs
  where
    go a b = a <+> separator <+> b

testProp :: LExprProp
testProp =
  Prop
    [ -- Step 0
      PointsTo (Current (unitLocus "q" 0)) (LSimpleExpr (Var "a"))
    , PointsTo (Current (unitLocus "q" 1)) (LSimpleExpr (Var "b"))

      -- Step 1
    , PointsTo (Step (Current (unitLocus "q" 0))) (LApply (Current (unitLocus "q" 0)) ("x" :=> Hadamard "x"))
    , Unchanged (Step (Current (unitLocus "q" 1)))

      -- Step 2
    , Unchanged (Step (Step (Current (unitLocus "q" 0))))
    , PointsTo (Step (Step (Current (unitLocus "q" 1))))
        $ LIfThenElse (LocusVar (Step (Current (unitLocus "q" 0))))
            (LApply (Step (Current (unitLocus "q" 1))) ("x" :=> Xor (Var "x") (Lit 1)))
            (LocusVar (Step (Current (unitLocus "q" 1))))
    ]

