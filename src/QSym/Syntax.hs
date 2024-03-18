module QSym.Syntax
  where

import QSym.Utils

data Expr
  = SKIP Posi 
  | X Posi
  | CU Posi Expr
  | RZ Int Posi
  | RRZ Int Posi
  | SR Int Var
  | SRR Int Var
  | Lshift Var
  | Rshift Var
  | Rev Var
  | QFT Var Int
  | RQFT Var Int
  | Seq Expr Expr

instance Semigroup Expr where
  (<>) = Seq

