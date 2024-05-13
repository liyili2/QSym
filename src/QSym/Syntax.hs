module QSym.Syntax
  where

import QSym.Utils
import Data.Bits hiding (xor, rotate, rotateR)
import qualified Data.Bits as Bits

data AExp
  = ANum Int | AVar Var | Minus AExp AExp | Plus AExp AExp | Mult AExp AExp
  deriving (Show, Eq)

data Posi = -- posi is x[v] where x is a quantum variable and v is an aexp
  Posi
    { posiVar :: Var
    , posiInt :: AExp
    }
  deriving (Show, Eq)

getPosiVar :: Posi -> Var
getPosiVar (Posi x _) = x

nextPos :: Posi -> Posi
nextPos (Posi x i) = Posi x (Plus i (ANum 1))


substAExp :: AExp -> Var -> Int -> AExp
substAExp (ANum n) x v = (ANum n)
substAExp (AVar x) y v = if x == y then ANum v else (AVar x)
substAExp (Minus e1 e2) x v = Minus (substAExp e1 x v) (substAExp e2 x v)
substAExp (Plus e1 e2) x v = Plus (substAExp e1 x v) (substAExp e2 x v)
substAExp (Mult e1 e2) x v = Mult (substAExp e1 x v) (substAExp e2 x v)

simpleAExp :: AExp -> AExp
simpleAExp (ANum v) = ANum v
simpleAExp (AVar x) = AVar x
simpleAExp (Minus e1 e2) = case simpleAExp e1 of
                                     ANum v1 -> case simpleAExp e2 of
                                          ANum v2 -> ANum (v1 - v2)
                                          _ -> Minus (ANum v1) e2
                                     _ -> Minus e1 e2
simpleAExp (Plus e1 e2) = case simpleAExp e1 of
                                     ANum v1 -> case simpleAExp e2 of
                                          ANum v2 -> ANum (v1 + v2)
                                          _ -> Plus (ANum v1) e2
                                     _ -> Plus e1 e2                                 
simpleAExp (Mult e1 e2) = case simpleAExp e1 of
                                     ANum v1 -> case simpleAExp e2 of
                                          ANum v2 -> ANum (v1 * v2)
                                          _ -> Mult (ANum v1) e2
                                     _ -> Mult e1 e2       
                                     
data BExp
  = BValue Bool
  | GBit AExp AExp
  | BLt AExp AExp
  | BEq AExp AExp
  | BNot BExp
  
substBExp :: BExp -> Var -> Int -> BExp
substBExp (GBit e1 e2) x v = GBit (substAExp e1 x v) (substAExp e2 x v)
substBExp (BLt e1 e2) x v = BLt (substAExp e1 x v) (substAExp e2 x v)
substBExp (BEq e1 e2) x v = BEq (substAExp e1 x v) (substAExp e2 x v)
substBExp (BNot e1) x v = BNot (substBExp e1 x v)


simpleBExp :: BExp -> BExp
simpleBExp (BValue v) = BValue v
simpleBExp (GBit e1 e2) = case simpleAExp e1 of
                                     ANum v1 -> case simpleAExp e2 of
                                          ANum v2 -> BValue (testBit v1 v2)
                                          _ -> GBit (ANum v1) e2
                                     _ -> GBit e1 e2    
simpleBExp (BLt e1 e2) = case simpleAExp e1 of
                                     ANum v1 -> case simpleAExp e2 of
                                          ANum v2 -> BValue (v1 < v2)
                                          _ -> BLt (ANum v1) e2
                                     _ -> BLt e1 e2    
simpleBExp (BEq e1 e2) = case simpleAExp e1 of
                                     ANum v1 -> case simpleAExp e2 of
                                          ANum v2 -> BValue (v1 == v2)
                                          _ -> BEq (ANum v1) e2
                                     _ -> BEq e1 e2    
simpleBExp (BNot e1) = case simpleBExp e1 of
                                     BValue v1 -> BValue (not v1)
                                     _ -> BNot e1
                                     
data Expr
  = SKIP Posi  -- do nothing
  | X Posi  -- x[v] posi qubit |0> -> |1> and |1> -> |0>, x is Nor type
  | CU Posi Expr  -- CU on x[v] posi qubit to control if we apply expr, x is Nor type.
  | RZ AExp Posi -- RZ on x[v] posi qubit to rotate AExp degree, x is Nor type
  | RRZ AExp Posi -- RRZ on x[v] posi qubit to rotate -AExp degree, x is Nor type
  | SR AExp Var -- SR on x array qubit to rotate a series of AExp degree, x is Phi type
  | SRR AExp Var -- SRR on x array qubit to rotate a series of -AExp degree, x is Phi type
  | Lshift Var  -- left-shift on x array, x is Nor type
  | Rshift Var  -- right shift-on x array, x is Nor type
  | Rev Var     -- rev on x array, x is nor type
  | QFT Var AExp -- QFT x i means "for array named x, change its type from Nor to Phi i"
  | RQFT Var AExp -- RQFT x i means "for array named x, change its type from Phi i to Nor"
  | Seq Expr Expr
  | App Var [AExp]
  | Fix Var Var [Var] Expr  -- fixed point op assumes variable closed, so every variable in expr are bonuded by Var, Var, and [Var]
  | IFExp BExp Expr Expr
  -- deriving (Show)
  
substExpr :: Expr -> Var -> Int -> Expr
substExpr (SKIP p) x v = SKIP (p {posiInt = (substAExp (posiInt p) x v)})
substExpr (X p) x v = X (p {posiInt = (substAExp (posiInt p) x v)})
substExpr (CU p e) x v = CU (p {posiInt = (substAExp (posiInt p) x v)}) (substExpr e x v)
substExpr (RZ q p) x v = RZ (substAExp q x v) (p {posiInt = (substAExp (posiInt p) x v)})
substExpr (RRZ q p) x v = RRZ (substAExp q x v) (p {posiInt = (substAExp (posiInt p) x v)})
substExpr (SR q y) x v = SR (substAExp q x v) y
substExpr (SRR q y) x v = SRR (substAExp q x v) y
substExpr (Lshift y) x v = Lshift y
substExpr (Rshift y) x v = Rshift y
substExpr (Rev y) x v = Rev y
substExpr (QFT y q) x v = QFT y (substAExp q x v)
substExpr (RQFT y q) x v = RQFT y (substAExp q x v)
substExpr (Seq e1 e2) x v = Seq (substExpr e1 x v) (substExpr e2 x v)
substExpr (App y el) x v = App y (map (\ a ->  substAExp a x v) el)
substExpr (Fix a b c e) x v = Fix a b c e
substExpr (IFExp b e1 e2) x v = IFExp (substBExp b x v) (substExpr e1 x v) (substExpr e2 x v)

simpExpr (SKIP p) = SKIP (p {posiInt = (simpleAExp (posiInt p))})
simpExpr (X p) = X (p {posiInt = (simpleAExp (posiInt p))})
simpExpr (CU p e) = CU (p {posiInt = (simpleAExp (posiInt p))}) (simpExpr e)
simpExpr (RZ q p) = RZ (simpleAExp q) (p {posiInt = (simpleAExp (posiInt p))})
simpExpr (RRZ q p) = RRZ (simpleAExp q) (p {posiInt = (simpleAExp (posiInt p))})
simpExpr (SR q y) = SR (simpleAExp q) y
simpExpr (SRR q y) = SRR (simpleAExp q) y
simpExpr (Lshift y) = Lshift y
simpExpr (Rshift y) = Rshift y
simpExpr (Rev y) = Rev y
simpExpr (QFT y b) = QFT y (simpleAExp b)
simpExpr (RQFT y b) = RQFT y (simpleAExp b)
simpExpr (Seq e1 e2) = Seq (simpExpr e1) (simpExpr e2)
simpExpr (App x el) = App x (map (\a -> simpleAExp a) el)
simpExpr (Fix x y z e) = Fix x y z e
simpExpr (IFExp b e1 e2) = IFExp (simpleBExp b) (simpExpr e1) (simpExpr e2)


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

