{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module QSym.Logic.SMT
  (SMT
  ,Symbol
  ,symbol

  ,and'
  ,or'

  ,pointsTo
  ,emp
  ,sep
  ,wand

  ,not'
  ,eq
  ,int
  ,add

  ,setLogic

  ,declareHeap
  ,declareConst
  ,setInfo

  ,checkSAT
  )
  where

import QSym.Logic.Syntax

import Prettyprinter hiding (sep)

import Data.Coerce
import Data.String

data SExpr
  = Atom String
  | List [SExpr]
  -- | String String
  | Bool Bool
  | Int Int
  deriving (Show)

data SMT a
  = Decl SExpr
  | Assert SExpr
  | SExpr SExpr

data Symbol
data Decl

data SomeSMT = forall a. SomeSMT (SMT a)

instance IsString SExpr where
  fromString = Atom

instance IsString (SMT a) where
  fromString = SExpr . fromString

instance IsString SomeSMT where
  fromString = SomeSMT . fromString

symbol :: String -> SMT a --Symbol
symbol = SExpr . Atom

nil :: SMT Symbol
nil = SExpr (Int 0)

newtype Block = Block [SomeSMT]
  deriving (Semigroup, Monoid)

one :: SMT a -> Block
one x = Block [SomeSMT x]

smtBlock :: [SMT a] -> Block
smtBlock = coerce Block . map SomeSMT

-- NOTE: Do not export
apply :: String -> [SExpr] -> SExpr
apply f xs = List (Atom f : xs)

assert :: SMT Bool -> SMT Decl
assert (SExpr e) = Assert e

and' :: [SMT Bool] -> SMT Bool
and' = SExpr . apply "and" . map toSExpr

or' :: [SMT Bool] -> SMT Bool
or' = SExpr . apply "or" . map toSExpr

pointsTo :: SMT a -> SMT a -> SMT Bool
pointsTo lhs rhs = SExpr $ apply "pto" [toSExpr lhs, toSExpr rhs]

emp :: SMT Bool
emp = SExpr $ Atom "sep.emp"

sep :: [SMT Bool] -> SMT Bool
sep = SExpr . apply "sep" . map toSExpr

wand :: SMT Bool -> SMT Bool -> SMT Bool
wand lhs rhs = SExpr (apply "wand" [toSExpr lhs, toSExpr rhs])

setLogic :: String -> SMT Decl
setLogic x = Decl $ apply "set-logic" [fromString x]

declareHeap :: String -> String -> SMT Decl
declareHeap lhs rhs = Decl $ apply "declare-heap" [fromString lhs, fromString rhs]

declareConst :: String -> String -> SMT Decl
declareConst name ty = Decl $ apply "declare-const" [fromString name, fromString ty]

not' :: SMT Bool -> SMT Bool
not' = SExpr . apply "not" . (:[]) . toSExpr

eq :: SMT a -> SMT a -> SMT Bool
eq x y = SExpr (apply "=" [toSExpr x, toSExpr y])

int :: Int -> SMT Int
int = SExpr . Int

add :: SMT Int -> SMT Int -> SMT Int
add x y = SExpr $ apply "+" [toSExpr x, toSExpr y]

setInfo :: [(String, String)] -> SMT Decl
setInfo = Decl . apply "set-info" . map fromString . flatten
  where
    flatten :: [(a, a)] -> [a]
    flatten = concat . map (\(x, y) -> [x, y])

checkSAT :: SMT Decl
checkSAT = Decl $ apply "check-sat" []

-- NOTE: Do not export
toSExpr :: SMT a -> SExpr
toSExpr (SExpr e) = e

instance Pretty SExpr where
  pretty (Atom x) = pretty x
  pretty (List xs) = parens $ hsep $ map pretty xs
  pretty (Bool b) = pretty b
  pretty (Int i) = pretty i

instance Pretty (SMT a) where
  pretty (Decl x) = pretty x
  pretty (Assert x) = pretty (List [Atom "assert", x])
  pretty (SExpr x) = pretty x

instance Pretty SomeSMT where
  pretty (SomeSMT x) = pretty x

instance Pretty Block where
  pretty (Block xs) = vcat $ map pretty xs

-- convertProp :: LExprProp -> SBool
-- convertProp (Prop xs) = foldr (.&&) sTrue (map convertConjunct xs)
--
-- convertConjunct :: LExprConjunct -> SBool
-- convertConjunct (PointsTo lhs rhs) = undefined
--
-- convertSteppedLocus :: SteppedLocus -> 
--
-- -- class ToSMT a where
-- --   toSMT :: a -> SBV a
--
--
--
