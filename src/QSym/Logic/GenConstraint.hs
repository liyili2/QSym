module QSym.Logic.GenConstraint
  (astConstraints
  )
  where

import QSym.Logic.Syntax
import QSym.Logic.SMT

import Qafny.Syntax.AST hiding (Range (..))
import qualified Qafny.Syntax.AST as Qafny
import Qafny.Syntax.Subst

import Data.Sum

import Control.Monad.Reader
import Data.String

import Prettyprinter

import Debug.Trace

newtype Gen a = Gen { getGen :: Reader Env a }
  deriving (Functor, Applicative, Monad, MonadReader Env)

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

astConstraints :: Int -> AST -> HighLevelSMT Bool
astConstraints bitSize =
  and' . map (toplevelConstraints bitSize)

toplevelConstraints :: Int -> Toplevel () -> HighLevelSMT Bool
toplevelConstraints bitSize (Toplevel (Inl qm)) =
  case qmBody qm of
    Nothing -> true
    Just block -> runGen (blockListConstraints (inBlock block)) (buildEnv bitSize qm)

blockListConstraints :: [Stmt ()] -> Gen (HighLevelSMT Bool)
blockListConstraints [] = pure true
blockListConstraints (x:xs) = do
  prop <- blockConstraints x
  rest <- traverse (fmap (varMap step) . blockConstraints) xs
  pure (prop ^&&^ and' rest)

blockConstraints :: Stmt () -> Gen (HighLevelSMT Bool)
blockConstraints (SAssert {}) = pure true -- TODO: Should we handle this?
blockConstraints (SCall f xs) = error "SCall"
blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement
blockConstraints (lhs :*=: rhs@(ELambda lam)) = do
  -- Add an apply constraint to the locus that was modified
  -- and unchanged constraints to the others

  -- create the apply expression
  let locus = toLocus lhs
  -- let apply_expr = undefined --LApply (Current locus) ("x" :=> (convertApplyExpr "x" rhs))
  -- For BellPair, all we have to do is inverse the locus TODO: actually get all the variables in the environment
  -- read the env (contains the function inputs and outputs, needed for the unchanged prop)
  env <- ask
  let bitSize = envBitSize env

  pure $ smtMap bitSize (\i v -> convertLambda lam (Current locus) i)
           (LocusName (Step (Current locus))) (LocusName (Current locus))

  -- pure $ smtMap bitSize (\i v -> convertExpr [(locus, i)] v)
  --          (LocusName (Step (Current locus))) (LocusName (Current locus))

  -- pure $ smtMapList (\i -> undefined)
  --          (LocusName (Step (Current locus))) (LocusName (Current locus))
  --          (map int [0..bitSize-1])

  -- add the unchanged properties
  -- let unchanged = enumerateUnchanged (allBindings env) locus
  -- pure undefined -- $ Prop ([Unchanged (Step (Current (invLocus locus)))] ++ [PointsTo (Step (Current locus)) apply_expr])
blockConstraints (SDafny _) = pure true
blockConstraints (SIf cond part body) = do

  bodyConstraints <- blockListConstraints (inBlock body)

  let cond' = convertGuardExp cond

  pure $ ifThenElse cond'
            bodyConstraints
            (and' (map unchanged (getNames bodyConstraints)))

  -- pure undefined -- $ Prop [If (LSimpleExpr (fromGuard cond)) undefined]
  where
    fromGuard (GEPartition p Nothing) = toLocusExpr p
    -- TODO: Is this right?
    fromGuard (GEPartition _ (Just e)) = convertExpr e Nothing
blockConstraints s = error $ "unimplemented: " ++ show s

unchanged :: Name -> HighLevelSMT Bool
unchanged x = eq (symbol x) (symbol (step x))

applyLambda :: LambdaF (Exp ()) -> Exp () -> Exp ()
applyLambda (LambdaF { bBases = [paramVar], eBases = [body] }) arg =
  subst [(paramVar, arg)] body

toLocus :: Partition -> Locus
toLocus (Partition xs) = Locus $ map convertRange xs

toLocusExpr :: Partition -> HighLevelSMT Int
toLocusExpr = symbol . LocusName . Current . toLocus

convertRange :: Qafny.Range -> Range
convertRange (Qafny.Range x start end) = Range x (convertSimpleExpr start) (convertSimpleExpr end)

convertLambda :: LambdaF (Exp ()) -> SteppedLocus -> Int -> HighLevelSMT Int
convertLambda (LambdaF { bBases = [paramVar], eBases = [body] }) locus ix =
  convertExpr body (Just (paramVar, at (LocusName locus) ix))

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

