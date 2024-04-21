module QSym.Syntax
  where

import QSym.Utils

data AExp
  = Int | Var | Minus AExp AExp | Plus AExp AExp

data BExp
  = GBit AExp AExp
  | BLt AExp AExp
  | BEq AExp AExp

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
  | QFT Var Int -- QFT x i means "for array named x, change its type from Nor to Phi i"
  | RQFT Var Int -- RQFT x i means "for array named x, change its type from Phi i to Nor"
  | Seq Expr Expr
  | App Var [AExp]
  | Fix Var Int [Var] Expr
  | IFExp BExp Expr Expr
  -- deriving (Show)

pprExpr :: Expr -> String
pprExpr (SKIP p) = "SKIP " ++ show p
pprExpr (X p) = "X " ++ show p
pprExpr (CU p e) =
  unlines
    ["CU " ++ show p ++ " ["
    ,unlines $ map ("  "++) $ lines (pprExpr e)
    ,"]"
    ]
pprExpr (RZ i p) = "RZ " ++ show i ++ ", " ++ show p
pprExpr (RRZ i p) = "RRZ " ++ show i ++ ", " ++ show p
pprExpr (SR i p) = "SR " ++ show i ++ ", " ++ show p
pprExpr (SRR i p) = "SRR " ++ show i ++ ", " ++ show p
pprExpr (Lshift v) = "Lshift " ++ show v
pprExpr (Rshift v) = "Rshift " ++ show v
pprExpr (Rev v) = "Rev " ++ show v
pprExpr (QFT v i) = "QFT " ++ show v ++ ", " ++ show i
pprExpr (RQFT v i) = "RQFT " ++ show v ++ ", " ++ show i
pprExpr (Seq a b) =
  let seqs = getSeqs a <> getSeqs b
  in
  unlines $ map pprExpr seqs

getSeqs :: Expr -> [Expr]
getSeqs (Seq a b) = getSeqs a <> getSeqs b
getSeqs e = [e]

instance Semigroup Expr where
  (<>) = Seq

