{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module QSym.Logic.Backend.Dafny
  where

import QSym.Logic.IR hiding (getBitRange, invertBitVec)
import QSym.Logic.Memory

import Control.Monad.State

import Prettyprinter

-- TODO: Invariants
-- TODO: We need an initial pass to determine how nested the bit vectors
-- should be. We did this with the SMT backend as well.
sumToDafny :: Doc ann -> Doc ann -> Int -> Sum -> Fresh (Doc ann)
sumToDafny oldMem newMem bitSize (Sum bounds f) =
  genFor' (0, bitSize) invariants $ \oldMemIx ->
    genFors (map (1,) bounds) invariants $ \newIxs ->
      let controlledVec = f (var (show oldMemIx)) (map (var . show) newIxs)
      in
      pure $ translateControlledVec newMem controlledVec
  where
    invariants = mempty -- TODO: what do we do here?

translateControlledVec :: Doc ann -> Controlled EVec -> Doc ann
translateControlledVec newMem (Controlled cond body) =
  translateControlCond cond (genMemoryUpdate newMem (genExpr body))

translateControlCond :: Expr Bool -> Doc ann -> Doc ann
translateControlCond cond rest
  | isTrue cond = rest
  | otherwise =
      genIf (genExpr cond) rest

genMemoryUpdate :: Doc ann -> Doc ann -> Doc ann
genMemoryUpdate newMem updateExpr = newMem =: updateExpr

genExpr :: Expr a -> Doc ann
genExpr (BoolLit True) = pretty "true"
genExpr (BoolLit False) = pretty "false"
genExpr (Equal a b) = binOp "==" (genExpr a) (genExpr b)
genExpr (And as) = genAnds as
genExpr (Or as) = genOrs as
genExpr (Not a) = pretty "!" <+> parens (genExpr a)
genExpr (Add a b) = binOp "+" (genExpr a) (genExpr b)
genExpr (Mul a b) = binOp "*" (genExpr a) (genExpr b)
genExpr (Sub a b) = binOp "-" (genExpr a) (genExpr b)
genExpr (Sqrt a) = pretty "sqrt" <> parens (pretty a)
genExpr (AmpFactor n) = pretty "((1/sqrt(2))^" <> pretty n <> pretty ")"
genExpr (Var x) = pretty x

genExpr (GetAmp (Var x)) = pretty (getAmpName x)
genExpr (GetPhase (Var x)) = pretty (getPhaseName x)
genExpr (GetBitVec (Var x)) = pretty (getBitVecName x)

genExpr (GetAmp (MkVec x _ _)) = genExpr x
genExpr (GetPhase (MkVec _ y _)) = genExpr y
genExpr (GetBitVec (MkVec _ _ z)) = genExpr z

genExpr (GetBit x i) = genExpr x <> brackets (pretty i)

genExpr (OverwriteBits origBV startIx newBV) =
  overwriteBitVec (genExpr origBV) startIx (genExpr newBV)

genExpr (GetBitRange x start end) =
  getBitRange (genExpr x) start end

genExpr (InvertBitVec x) =
  invertBitVec (genExpr x)

genExpr (ToInt x) = genExpr x
genExpr (FromInt x) = genExpr x
genExpr (IntLit x) = pretty x

overwriteBitVec :: Doc ann -> Int -> Doc ann -> Doc ann
overwriteBitVec origBV startIx newBV =
  origBV <> brackets (pretty ".." <> pretty startIx)
         <+> pretty "+"
         <+> newBV
         <+> pretty "+"
         <+> origBV <> brackets (pretty startIx <+> pretty "+" <+> pretty "|" <> newBV <> pretty "|")

getBitRange :: Doc ann -> Int -> Int -> Doc ann
getBitRange x start end =
  x <> brackets (pretty start <> pretty ".." <> pretty end)

invertBitVec :: Doc ann -> Doc ann
invertBitVec x = pretty "invertBitVec" <> parens x

invertBitVecDef :: Doc ann
invertBitVecDef =
  vsep
    [pretty "function" <+> pretty "invertBitVec" <> parens (pretty "bitVector:" <+> pretty "seq<bv1>") <> pretty ":" <+> pretty "seq<bv1>"
    , braces
        (indent 2 (pretty "seq(|bitVector|, i requires 0 <= i < |bitVector| => if bitVector[i] == 0b1 then 0b0 else 0b1)"))
    ]

genAnds :: [Expr a] -> Doc ann
genAnds = genOps "&&" (BoolLit True)

genOrs :: [Expr a] -> Doc ann
genOrs = genOps "||" (BoolLit False)

genOps :: String -> Expr a -> [Expr b] -> Doc ann
genOps op z [] = genExpr z
genOps op z [x] = genExpr x
genOps op z (x:xs) = binOp op (genExpr x) (genOps op z xs)

getBitVecName :: String -> String
getBitVecName = (<> "BitVec")

getPhaseName :: String -> String
getPhaseName = (<> "Phase")

getAmpName :: String -> String
getAmpName = (<> "Amp")

genIf :: Doc ann -> Doc ann -> Doc ann
genIf c t =
  pretty "if" <+> c <+> braces t

genFors :: forall ann.
  [(Int, Int)] ->
  (Doc ann -> Doc ann) ->
  ([Doc ann] -> Fresh (Doc ann)) ->
  Fresh (Doc ann)
genFors bounds invariants body =
  go [] bounds
  where
    go vars [] = body vars

    go vars (b:bs) =
      genFor' b invariants $ \x ->
        go (vars ++ [x]) bs


genFor' ::
  (Int, Int) ->
  (Doc ann -> Doc ann) ->
  (Doc ann -> Fresh (Doc ann)) ->
  Fresh (Doc ann)
genFor' bound f g = genFor bound go
  where
    go x = do
      b <- g x
      pure (f x, b)

genFor ::
  (Int, Int) ->
  (Doc ann -> Fresh (Doc ann, Doc ann)) -> -- (invariant, body)
  Fresh (Doc ann)
genFor (start, end) f = do
  x <- fresh

  let varX = var' x

  (invariant, body) <- f varX

  pure $ vsep
    [stmt $ varX =: pretty start
    ,while (x .< pretty end)
      invariant
      body
    ]

-- genFn ::
--   (Doc ann -> Doc ann) ->

while :: Doc ann -> Doc ann -> Doc ann -> Doc ann
while cond invariants body =
  vsep
    [pretty "while" <+> cond
    ,indent 2 invariants
    ,braces (indent 2 body)
    ]

var' :: Doc ann -> Doc ann
var' x = pretty "var" <+> x

binOp :: String -> Doc ann -> Doc ann -> Doc ann
binOp op x y = hsep [parens x, pretty op, parens y]

(=:) :: Doc ann -> Doc ann -> Doc ann
x =: y = hsep [x, pretty ":=", y]

(.<) :: Doc ann -> Doc ann -> Doc ann
(.<) = binOp "<"

stmt :: Doc ann -> Doc ann
stmt d = d <> pretty ";"


----

newtype DafnyGen a =
    DafnyGen (State Memory a)
  deriving (Functor, Applicative, Monad, MonadState Memory)


----


newtype Fresh a = Fresh (State Int a)
  deriving (Functor, Applicative, Monad)

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0

fresh :: Fresh (Doc ann)
fresh = do
  n <- Fresh get
  Fresh $ modify (+1)
  pure (pretty (baseName ++ show n))
  where
    baseName = "x"
