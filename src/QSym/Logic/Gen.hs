module QSym.Logic.Gen
  where

import Qafny.Syntax.AST hiding (Range (..), Block)
import qualified Qafny.Syntax.AST as Qafny
import Qafny.Syntax.Subst

import QSym.Logic.SMT
import QSym.Logic.Syntax

import Data.String
import Control.Monad.Reader

import Data.List
import Data.Function
import Data.Foldable (fold)

import Prettyprinter

newtype Gen a = Gen { getGen :: Reader Env a }
  deriving (Functor, Applicative, Monad, MonadReader Env)

currentVar :: Var -> Name
currentVar x = VarName (Current x)

type HighLevelSMT = SMT Name

data Name = LocusName SteppedLocus | VarName (Stepped Var) | BuiltinName String
  deriving (Show)

instance Pretty Name where
  pretty (LocusName x) = pretty x
  pretty (VarName x) = pretty x
  pretty (BuiltinName x) = pretty x

instance IsString Name where
  fromString = BuiltinName

instance Steppable Name where
  step (LocusName x) = LocusName (step x)
  step (VarName x) = VarName (step x)
  step (BuiltinName x) = BuiltinName x

data Env =
  Env
  { envInputs :: Bindings ()
  , envOutputs :: Bindings ()
  , envBitSize :: Int
  , envVarAssignment :: [(String, Int)] -- | Loci variable -> bit vector (base) index
  }

getVarBaseIndex :: String -> Gen Int
getVarBaseIndex v = do
  asn <- fmap envVarAssignment ask
  let Just i = lookup v asn
  pure i

rangeToPhysicalIndices :: Qafny.Range -> Gen (Int, Int)
rangeToPhysicalIndices (Qafny.Range x (ENum start) (ENum end)) = do
  base <- getVarBaseIndex x
  pure (start + base, end + base)

-- | Maximum size associated to a variable name, given by the loci
newtype LociSizes = LociSizes [(String, Int)]
  deriving (Show)

oneLociSize :: String -> Int -> LociSizes
oneLociSize v i = LociSizes [(v, i)]

instance Semigroup LociSizes where
  LociSizes a0 <> LociSizes b0 = LociSizes $ go a0 b0
    where
      go a [] = a
      go xs (y@(v, i):ys) =
        case lookup v xs of
          Nothing -> y : (go xs ys)
          Just j -> (v, max i j) : go (deleteBy ((==) `on` fst) y xs) ys

instance Monoid LociSizes where
  mempty = LociSizes mempty

getOtherInputs :: [Int] -> Gen [Int]
getOtherInputs usedInputs = do
  env <- ask

  let allInputs = zipWith const [0..] $ envInputs env

  pure $ filter (`notElem` usedInputs) allInputs

-- |allBindings returns a list of both the inputs and the outputs from the environment
allBindings :: Env -> Bindings ()
allBindings env = envInputs env ++ envOutputs env

buildEnv :: Int -> QMethod () -> Env
buildEnv bitSize qm = Env (qmInputs qm) (qmOutputs qm) bitSize (allocateIndices (getLociSizes qm))

allocateIndices :: LociSizes -> [(String, Int)]
allocateIndices (LociSizes xs0) = go 0 xs0
  where
    go _ [] = []
    go currIndex ((v, size) : xs) =
      (v, currIndex) : go (currIndex + size) xs

-- For now, we just get loci where the range is given by integer
-- literals
-- TODO: Generalize this to work with indexing expressions that have
-- variables in them
getLociSizes :: QMethod () -> LociSizes
getLociSizes x =
  case qmBody x of
    Nothing -> mempty
    Just body -> mconcat $ map getLociSizesStmt $ inBlock body

getLociSizesStmt :: Stmt () -> LociSizes
getLociSizesStmt (SAssert {}) = mempty
getLociSizesStmt (SCall _ xs) = mconcat $ map getLociSizesExp xs
getLociSizesStmt (SVar _ x) = fold $ fmap getLociSizesExp x
getLociSizesStmt (lhs ::=: rhs) = getLociSizesExp rhs
getLociSizesStmt (SIf (GEPartition part Nothing) part' (Qafny.Block xs)) =
  toLociSizes part
    <> toLociSizes part'
    <> mconcat (map getLociSizesStmt xs)

toLociSizes :: Partition -> LociSizes
toLociSizes (Partition xs) = mconcat (map rangeToLociSizes xs)

rangeToLociSizes :: Qafny.Range -> LociSizes
rangeToLociSizes (Qafny.Range x _start end) = fold $ do
  endNum <- getNum end
  Just (oneLociSize x endNum)

-- TODO: Finish
getLociSizesExp :: Exp () -> LociSizes
getLociSizesExp (ENum _) = mempty
getLociSizesExp (EVar x) = mempty
getLociSizesExp (EOp2 _ x y) = getLociSizesExp x <> getLociSizesExp y

toLocus :: Partition -> Locus
toLocus (Partition xs) = Locus $ map convertRange xs

convertRange :: Qafny.Range -> Range
convertRange (Qafny.Range x start end) = Range x (convertSimpleExpr start) (convertSimpleExpr end)

getNum :: Exp () -> Maybe Int
getNum (ENum i) = Just i
getNum _ = Nothing

-- convertLambda :: LambdaF (Exp ()) -> SteppedLocus -> Int -> HighLevelSMT Int
-- convertLambda (LambdaF { bBases = [paramVar], eBases = [body] }) locus ix =
--   convertExpr body (Just (paramVar, select (symbol (LocusName locus)) (int ix)))

convertSimpleExpr :: Exp () -> SimpleExpr
convertSimpleExpr (ENum i) = Lit i
convertSimpleExpr (EVar x) = Var x
convertSimpleExpr (EOp2 OAdd x y) = Add (convertSimpleExpr x) (convertSimpleExpr y)
-- allocateIndicesBlock :: [(String, Int)] -> [Stmt ()] -> [(String, Int)]
-- allocateIndicesBlock soFar [] = soFar
-- allocateIndicesBlock soFar (x:xs) =
--   let newAlloc = allocateIndicesStmt soFar x
--   in
--   allocateIndicesStmt newAlloc xs
--
-- allocateIndicesStmt :: [(String, Int)] -> Stmt () -> [(String, Int)]
-- allocateIndicesStmt = undefined

runGen :: Gen a -> Env -> a
runGen (Gen g) = runReader g

getSteppedVar :: Var -> [Name] -> [Stepped Var]
getSteppedVar x [] = []
getSteppedVar x (VarName y:ys)
  | getSteppedName y == x = y : getSteppedVar x ys
getSteppedVar x (_:ys) = getSteppedVar x ys

