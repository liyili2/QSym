{-# LANGUAGE OverloadedStrings #-}

module QSym.Logic.GenConstraint
  (smtPreamble
  ,astConstraints
  )
  where

import Prelude hiding (div)

import QSym.Logic.Syntax
import QSym.Logic.SMT

import Qafny.Syntax.AST hiding (Range (..), Block)
import qualified Qafny.Syntax.AST as Qafny
import Qafny.Syntax.Subst

import Data.Sum

import Control.Monad.Reader
import Data.String

import Prettyprinter

import Debug.Trace

heapType :: String
heapType = "(Array Int (Array Int Real))"

bitVecSize :: Int
bitVecSize = 4

bitVecType :: String
bitVecType = "(_ BitVec " ++ show bitVecSize ++ ")"

bitVecArrayType :: String
bitVecArrayType = "(Array Int " ++ bitVecType ++ ")"

bitVecLit :: String -> SMT Name Int
bitVecLit i = fromString $ "(_ bv" ++ i ++ " " ++ show bitVecSize ++ ")"

mkLoc :: Int -> SMT Name a
mkLoc i = fromString $ "q" ++ show i

-- getHeapName :: Int -> Name
-- getHeapName i = fromString $ "mem" ++ show i
--
-- getVecsName :: Int -> Name
-- getVecsName i = fromString $ "mem" ++ show i ++ "-vecs"

smtPreamble :: Block Name
smtPreamble =
  smtBlock
    [setLogic "ALL"
    ,setOption ":produce-models" "true"
    ,setOption ":pp.decimal" "true"
    ,declareConst "sqrt2" "Real"
    ,assert $ eq (mul "sqrt2" "sqrt2") (int 2)
    ,assert $ gt "sqrt2" (int 2)

    ,declareConst (currentVar "mem") (fromString heapType)
    ,declareConst (currentVar "mem-vecs") (fromString bitVecArrayType)
    ]

newtype Gen a = Gen { getGen :: Reader Env a }
  deriving (Functor, Applicative, Monad, MonadReader Env)

currentVar :: Var -> Name
currentVar x = VarName (Current x)

type HighLevelSMT = SMT Name

data Name = LocusName SteppedLocus | VarName (Stepped Var) | BuiltinName String

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
  }

-- |allBindings returns a list of both the inputs and the outputs from the environment
allBindings :: Env -> Bindings ()
allBindings env = envInputs env ++ envOutputs env

buildEnv :: Int -> QMethod () -> Env
buildEnv bitSize qm = Env (qmInputs qm) (qmOutputs qm) bitSize

runGen :: Gen a -> Env -> a
runGen (Gen g) = runReader g

astConstraints :: Int -> AST -> Block Name
astConstraints bitSize =
  mconcat . map (toplevelConstraints bitSize)

toplevelConstraints :: Int -> Toplevel () -> Block Name
toplevelConstraints bitSize (Toplevel (Inl qm)) =
  case qmBody qm of
    Nothing -> mempty
    Just block -> runGen (blockListConstraints (inBlock block)) (buildEnv bitSize qm)

blockListConstraints :: [Stmt ()] -> Gen (Block Name)
blockListConstraints [] = pure mempty
blockListConstraints (x:xs) = do
  prop <- blockConstraints x
  rest <- mconcat <$> traverse (fmap (varMapBlock step) . blockConstraints) xs
  pure (prop <> rest)

blockConstraints :: Stmt () -> Gen (Block Name)
blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
blockConstraints (SCall f xs) = error "SCall"
blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement
blockConstraints (lhs :*=: EHad) = do
  pure $ hadamard (partitionToName lhs) (currentVar "mem")
-- blockConstraints (lhs :*=: rhs@(ELambda lam)) = do
  -- -- Add an apply constraint to the locus that was modified
  -- -- and unchanged constraints to the others
  --
  -- -- create the apply expression
  -- let locus = toLocus lhs
  -- -- let apply_expr = undefined --LApply (Current locus) ("x" :=> (convertApplyExpr "x" rhs))
  -- -- For BellPair, all we have to do is inverse the locus TODO: actually get all the variables in the environment
  -- -- read the env (contains the function inputs and outputs, needed for the unchanged prop)
  -- env <- ask
  -- let bitSize = envBitSize env
  --
  -- pure $ smtMap bitSize (\i v -> convertLambda lam (Current locus) i)
  --          (LocusName (Step (Current locus))) (LocusName (Current locus))
  --
  -- -- pure $ smtMap bitSize (\i v -> convertExpr [(locus, i)] v)
  -- --          (LocusName (Step (Current locus))) (LocusName (Current locus))
  --
  -- -- pure $ smtMapList (\i -> undefined)
  -- --          (LocusName (Step (Current locus))) (LocusName (Current locus))
  -- --          (map int [0..bitSize-1])
  --
  -- -- add the unchanged properties
  -- -- let unchanged = enumerateUnchanged (allBindings env) locus
  -- -- pure undefined -- $ Prop ([Unchanged (Step (Current (invLocus locus)))] ++ [PointsTo (Step (Current locus)) apply_expr])
blockConstraints (SDafny _) = pure mempty
blockConstraints (SIf (GEPartition part Nothing) part' (Qafny.Block [x :*=: ELambda (LambdaF { eBases = [EOp2 OMod (EOp2 OAdd (EVar v) (ENum 1)) (ENum 2)] })])) = do
    pure $ cnot (partitionToName part) (partitionToName x) (currentVar "mem") (currentVar "mem-vecs")

  -- bodyConstraints <- blockListConstraints (inBlock body)
  --
  -- let cond' = convertGuardExp cond
  --
  -- pure $ ifThenElse cond'
  --           bodyConstraints
  --           undefined --(and' (map unchanged (getNames bodyConstraints)))
  --
  -- -- pure undefined -- $ Prop [If (LSimpleExpr (fromGuard cond)) undefined]
  -- where
  --   fromGuard (GEPartition p Nothing) = toLocusExpr p
  --   -- TODO: Is this right?
  --   fromGuard (GEPartition _ (Just e)) = convertExpr e Nothing
blockConstraints s = error $ "unimplemented: " ++ show s

-- unchanged :: Name -> HighLevelSMT Bool
-- unchanged x = eq (symbol x) (symbol (step x))

applyLambda :: LambdaF (Exp ()) -> Exp () -> Exp ()
applyLambda (LambdaF { bBases = [paramVar], eBases = [body] }) arg =
  subst [(paramVar, arg)] body

toLocus :: Partition -> Locus
toLocus (Partition xs) = Locus $ map convertRange xs

-- TODO: Partially hardcoded for now
partitionToName :: Partition -> Int
partitionToName (Partition [Qafny.Range x start end]) = evalToInt start

evalToInt :: Exp () -> Int
evalToInt (ENum i) = i

toLocusExpr :: Partition -> HighLevelSMT Int
toLocusExpr = symbol . LocusName . Current . toLocus

convertRange :: Qafny.Range -> Range
convertRange (Qafny.Range x start end) = Range x (convertSimpleExpr start) (convertSimpleExpr end)

convertLambda :: LambdaF (Exp ()) -> SteppedLocus -> Int -> HighLevelSMT Int
convertLambda (LambdaF { bBases = [paramVar], eBases = [body] }) locus ix =
  convertExpr body (Just (paramVar, select (symbol (LocusName locus)) (int ix)))

convertSimpleExpr :: Exp () -> SimpleExpr
convertSimpleExpr (ENum i) = Lit i
convertSimpleExpr (EVar x) = Var x
convertSimpleExpr (EOp2 OAdd x y) = Add (convertSimpleExpr x) (convertSimpleExpr y)

-- |convertExpr converts a Qafny AST Expression to a SimpleExpr that can be used for symbolic execution
convertExpr :: Exp () -> Maybe (Var, HighLevelSMT Int) -> HighLevelSMT Int
convertExpr e0 substMaybe = go e0
  where
    go (ENum i) = int i
    go (EVar x) =
      case substMaybe of
        Just (paramVar, arg)
          | x == paramVar -> arg
        _ -> symbol (VarName (Current x))
    go (EOp2 OAdd x y) = add (go x) (go y)

convertBoolExpr :: Exp () -> HighLevelSMT Bool
convertBoolExpr (ENum i) = bool (i /= 0)
convertBoolExpr (EVar x) = symbol (VarName (Current x))

convertGuardExp :: GuardExp -> HighLevelSMT Bool
convertGuardExp (GEPartition p eMaybe) =
    not' $ eq (symbol (LocusName (Current (toLocus p)))) (int 0)
  where
    eBool =
      case eMaybe of
        Just e -> convertBoolExpr e
        Nothing -> true

cnot :: Int -> Int -> Name -> Name -> Block Name
cnot i j mem memVecs =
  smtBlock
    [assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 0))
                 (select (select (symbol mem) (mkLoc i)) (int 0))
    ,assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 1))
                 (select (select (symbol mem) (mkLoc i)) (int 1))

    ,assert $ eq (select (select (symbol (step mem)) (mkLoc (i+1))) (int 0))
                 (select (select (symbol mem) (mkLoc (i+1))) (int 0))
    ,assert $ eq (select (select (symbol (step mem)) (mkLoc (i+1))) (int 1))
                 (select (select (symbol mem) (mkLoc (i+1))) (int 1))

    -- entangle
    -- TODO: Hardcoded right now
    ,assert $ eq (select (symbol memVecs) (int 0)) $ bitVecLit "00"
    ,assert $ eq (select (symbol memVecs) (int 0)) $ bitVecLit "11"
    ]

hadamard :: Int -> Name -> Block Name
hadamard i mem =
  smtBlock
    [assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 0)) (hadamardFirst 0 mem)
    ,assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 1)) (hadamardSecond 1 mem)
    ]

hadamardFirst :: Int -> Name -> SMT Name Int
hadamardFirst loc mem =
  div
    (add (select (select (symbol mem) (int loc)) (int 0))
         (select (select (symbol mem) (int loc)) (int 1)))
    "sqrt2"

hadamardSecond :: Int -> Name -> SMT Name Int
hadamardSecond loc mem =
  div
    (sub (select (select (symbol mem) (int loc)) (int 0))
         (select (select (symbol mem) (int loc)) (int 1)))
    "sqrt2"

unchanged :: Name -> SMT Name Decl
unchanged name =
  update name id

update :: Name -> (SMT Name a -> SMT Name a) -> SMT Name Decl
update name f =
  assert $ eq (symbol (step name)) (f (symbol name))

-- convertExpr env (ENum i) = int i
-- convertExpr env (EVar x) = symbol (VarName (Current x))
-- convertExpr env (EOp2 OAdd x y) = add (convertExpr x) (convertExpr y)

-- -- |convertApplyExpr converts a Qafny AST Expression used in an apply statement "*=" 
-- --  to a lambda that can be used in the LApply constructor.
-- --  where:
-- --    name of type `String` is the name to use for Hadamard gates and similar.
-- --    expression of type `Exp ()` is the expression tree to convert
-- --  return of type `SimpleExpr`
-- convertApplyExpr :: String -> Exp () -> HighLevelSMT Int
-- convertApplyExpr name EHad = Hadamard name

