{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module QSym.Logic.ToSMT
  (ToSMT
  ,toSMT
  )
  where

import QSym.Logic.SMT
import QSym.Logic.Syntax

import Data.List
import Data.Bifunctor

class ToSMT a b | a -> b where
  toSMT :: a -> SMT b

instance ToSMT Locus Int where
  toSMT = symbol . fromLocus

fromLocus :: Locus -> String
fromLocus = intercalate "__" . map fromRange . unLocus
  where
    fromRange (Range name (Lit start) (Lit end)) =
      name ++ "_" ++ show start ++ "_" ++ show end

instance ToSMT SteppedLocus Int where
  toSMT stepped =
    let (stepCount, x) = fromStepped stepped
    in
    symbol $ "_" ++ show stepCount ++ "_" ++ fromLocus x

fromStepped :: Stepped a -> (Int, a)
fromStepped (Current x) = (0, x)
fromStepped (Step x) =
  first (+1) (fromStepped x)

instance ToSMT a Int => ToSMT (Conjunct a) Bool where
  toSMT (PointsTo lhs rhs) = pointsTo (toSMT lhs) (toSMT rhs)
  toSMT (Unchanged locus) = eq (toSMT (Step locus)) (toSMT locus)

instance ToSMT a Int => ToSMT (Prop a) Bool where
  toSMT (Prop xs) = and' (map toSMT xs)

instance ToSMT SimpleExpr Int where
  toSMT (Add x y) = add (toSMT x) (toSMT y)
  toSMT (Lit i) = int i
  toSMT (LocusVar x) = toSMT x
  toSMT (Var x) = symbol $ "_v_" ++ show x

instance ToSMT LExpr Bool where
    -- TODO: Implement permutation
  toSMT (LApply (Step lhs) rhs) =
    eq
      (toSMT (Step lhs))
      (toSMT (lambdaApply rhs (LocusVar lhs)))

  toSMT (LIfThenElse cond t f) = undefined

  toSMT (LSimpleExpr e) = undefined

