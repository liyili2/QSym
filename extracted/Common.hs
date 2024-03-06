{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Common
  where

type Var = Nat
type Posi = Prod Var Nat

data Prod a b =
   Pair a b
  deriving (Eq)

type Nat = Int
-- data Nat =
--    O
--  | S Nat

pattern O :: Nat
pattern O = 0

pattern S :: Nat -> Nat
pattern S x <- x + 1
  where
    S x = x + 1

data Exp =
   SKIP Posi
 | X Posi
 | CU Posi Exp
 | RZ Nat Posi
 | RRZ Nat Posi
 | SR Nat Var
 | SRR Nat Var
 | Lshift Var
 | Rshift Var
 | Rev Var
 | QFT Var Nat
 | RQFT Var Nat
 | Seq Exp Exp

