{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module QSym.Logic.Backend.Dafny
  where

import QSym.Logic.IR hiding (getBitRange, invertBitVec)
import QSym.Logic.Memory

import qualified Qafny.Syntax.AST as Qafny

import Control.Monad.State

import Prettyprinter

type MemTypeDecl ann = (Doc ann, Doc ann, Doc ann)

-- TODO: implement loops
getSum :: LoopedSum -> Sum
getSum (NoLoop x) = x

loopedSumsToFunction :: String -> Int -> [LoopedSum] -> Doc ann
loopedSumsToFunction fnName bitSize looped = sumListToFunction fnName bitSize (map getSum looped)
  -- let (body, decls) = runFresh (loopedSumListToDafny bitSize sums)
  -- in
  -- mkMemFunction fnName decls body

sumListToFunction :: String -> Int -> [Sum] -> Doc ann
sumListToFunction fnName bitSize sums =
  let (body, decls) = runFresh (sumListToDafny bitSize sums)
  in
  mkMemFunction fnName decls body

mkMemFunction :: String -> [MemTypeDecl ann] -> Doc ann -> Doc ann
mkMemFunction fnName params body =
  vsep
    [pretty "function" <+> pretty fnName <> parens (typeDeclListParams params)
    ,braces (indent 2 body)
    ]

typeDeclListParams :: [MemTypeDecl ann] -> Doc ann
typeDeclListParams = hsep . punctuate (pretty ",") . concatMap typeDeclParams

typeDeclParams :: MemTypeDecl ann -> [Doc ann]
typeDeclParams (x, y, z) = punctuate (pretty ",") [x, y, z]

loopedSumToDafny :: Doc ann -> Doc ann -> Int -> LoopedSum -> Fresh (Int, Doc ann)
loopedSumToDafny oldMem newMem bitSize (NoLoop x) = sumToDafny oldMem newMem bitSize x
loopedSumToDafny oldMem newMem bitSize (ForIn range loopedSums) =
  loopedSumListToDafny oldMem newMem bitSize range loopedSums

loopedSumListToDafny :: Doc ann -> Doc ann -> Int -> Qafny.Range -> [LoopedSum] -> Fresh (Int, Doc ann)
loopedSumListToDafny oldMem newMem bitSize (Qafny.Range var (Qafny.ENum start) (Qafny.ENum end)) loopedSums = do
  results <- traverse (loopedSumToDafny oldMem newMem bitSize) loopedSums
  let (memSizes, sumsCode) = unzip results

  let sumsBody = vcat sumsCode

  theFor <- genForWithVar (pretty var) (start, end) $ \i ->
    let invariant = mempty -- TODO
    in
    pure (invariant, sumsBody)

  pure (head memSizes, theFor) -- TODO: Is head correct?

sumListToDafny :: Int -> [Sum] -> Fresh (Doc ann, [MemTypeDecl ann])
sumListToDafny bitSize =
  go 0
  where
    go currMemId [] = pure (mempty, mempty)
    go currMemId (sum:restSums) = do
      let oldMemBaseName = mkMemBaseName currMemId
          newMemBaseName = mkMemBaseName currMemId

      (memSize, sumCode) <- sumToDafny oldMemBaseName newMemBaseName bitSize sum

      let currDecl = mkMemDecls newMemBaseName memSize

      (restCode, restDecls) <- go (currMemId+1) restSums
      pure (vsep [sumCode, restCode]
           ,currDecl : restDecls)

-- TODO: Invariants
sumToDafny :: Doc ann -> Doc ann -> Int -> Sum -> Fresh (Int, Doc ann)
sumToDafny oldMem newMem bitSize (Sum bounds f) = do
    doc <- docM
    let memSize = length bounds
    pure (memSize, doc)
  where
    invariants = mempty -- TODO: what do we do here?
    docM =
      genFor' (0, bitSize) invariants $ \oldMemIx ->
        genFors (map (1,) bounds) invariants $ \newIxs ->
          let controlledVec = f (var (show oldMemIx)) (map (var . show) newIxs)
          in
          pure (translateControlledVec newMem controlledVec)

mkMemBaseName :: Int -> Doc ann
mkMemBaseName memId = pretty "mem" <> pretty memId <> pretty "_"

mkMemDecls :: Doc ann -> Int -> MemTypeDecl ann
mkMemDecls memName size =
  (mkDecl memName (mkBitVecType size)
  ,mkDecl memName (mkAmpType size)
  ,mkDecl memName (mkPhaseType size)
  )

mkDecl :: Doc ann -> Doc ann -> Doc ann
mkDecl var ty = var <> pretty ":" <+> ty

mkBitVecType :: Int -> Doc ann
mkBitVecType = mkVecType "bv1"

mkAmpType :: Int -> Doc ann
mkAmpType = mkVecType "real"

mkPhaseType :: Int -> Doc ann
mkPhaseType = mkVecType "real"

mkVecType :: String -> Int -> Doc ann
mkVecType elemType 0            = pretty elemType
mkVecType elemType nestingDepth = pretty "seq" <> pretty "<" <> mkVecType elemType (nestingDepth-1) <> pretty ">"

translateControlledVec :: Doc ann -> Controlled EVec -> Doc ann
translateControlledVec newMem (Controlled cond body) =
  translateControlCond cond (vsep (mkAssignments newMem body))

translateControlCond :: Expr Bool -> Doc ann -> Doc ann
translateControlCond cond rest
  | isTrue cond = rest
  | otherwise =
      genIf (genExpr cond) rest

-- genMemoryUpdate :: Doc ann -> Doc ann -> Doc ann
-- genMemoryUpdate newMem updateExpr = newMem =: updateExpr

mkAssignments :: Doc ann -> Expr EVec -> [Doc ann]
mkAssignments newMem e =
  [getAmpName newMem =: genExpr (GetAmp e)
  ,getPhaseName newMem =: genExpr (GetPhase e)
  ,getBitVecName newMem =: genExpr (GetBitVec e)
  ]

genExpr :: Pretty (SmtTy a) => Expr a -> Doc ann
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

genExpr (GetAmp (Var x)) = getAmpName (pretty x)
genExpr (GetPhase (Var x)) = getPhaseName (pretty x)
genExpr (GetBitVec (Var x)) = getBitVecName (pretty x)

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
genExpr (ScalarMult x y) =
  genExpr
    (MkVec (Mul x (GetAmp y))
           (GetPhase y)
           (GetBitVec y))

genExpr (Omega a b) =
  undefined
  -- pretty "realExp" <> parens (pretty "2" <+> pretty "*" <+> pretty "pi" <

genExpr e = error $ "unimplemented: " ++ show (pretty e)

omegaSpec :: Doc ann
omegaSpec =
  vsep
  [pretty "function realExp(r: real, e: int): real decreases if e > 0 then e else -e {"
  ,pretty "if e == 0 then r"
  ,pretty "else if e < 0 then realExp(r/10.0, e+1)"
  ,pretty "else realExp(r*10.0, e-1)"
  .pretty "}"
  ]

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

genAnds :: Pretty (SmtTy a) => [Expr a] -> Doc ann
genAnds = genOps "&&" (BoolLit True)

genOrs :: Pretty (SmtTy a) => [Expr a] -> Doc ann
genOrs = genOps "||" (BoolLit False)

genOps :: (Pretty (SmtTy a), Pretty (SmtTy b)) => String -> Expr a -> [Expr b] -> Doc ann
genOps op z [] = genExpr z
genOps op z [x] = genExpr x
genOps op z (x:xs) = binOp op (genExpr x) (genOps op z xs)

getBitVecName :: Doc ann -> Doc ann
getBitVecName = (<> pretty "BitVec")

getPhaseName :: Doc ann -> Doc ann
getPhaseName = (<> pretty "Phase")

getAmpName :: Doc ann -> Doc ann
getAmpName = (<> pretty "Amp")

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
genFor bounds f = do
  x <- fresh

  genForWithVar x bounds f

genForWithVar :: 
  Doc ann ->
  (Int, Int) ->
  (Doc ann -> Fresh (Doc ann, Doc ann)) -> -- (invariant, body)
  Fresh (Doc ann)
genForWithVar varX (start, end) f = do
  (invariant, body) <- f varX
  pure $ vsep
    [stmt $ varX =: pretty start
    ,while (varX .< pretty end)
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
