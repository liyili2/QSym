module QSym.Logic.GenConstraint
  (astConstraints
  )
  where

import QSym.Logic.Syntax

import Qafny.Syntax.AST hiding (Range (..))
import qualified Qafny.Syntax.AST as Qafny

import Data.Sum

import Control.Monad.Reader

import Debug.Trace

newtype Gen a = Gen { getGen :: Reader Env a }
  deriving (Functor, Applicative, Monad, MonadReader Env)

data Env =
  Env
  { envInputs :: Bindings ()
  , envOutputs :: Bindings ()
  }

allBindings :: Env -> Bindings ()
allBindings env = envInputs env ++ envOutputs env

buildEnv :: QMethod () -> Env
buildEnv qm = Env (qmInputs qm) (qmOutputs qm)

runGen :: Gen a -> Env -> a
runGen (Gen g) = runReader g

astConstraints :: AST -> LExprProp
astConstraints =
  mconcat . map toplevelConstraints

toplevelConstraints :: Toplevel () -> LExprProp
toplevelConstraints (Toplevel (Inl qm)) =
  case qmBody qm of
    Nothing -> mempty
    Just block -> runGen (blockListConstraints (inBlock block)) (buildEnv qm)

blockListConstraints :: [Stmt ()] -> Gen LExprProp
blockListConstraints [] = pure mempty
blockListConstraints (x:xs) = do
  prop <- blockConstraints x
  rest <- traverse (fmap step . blockConstraints) xs
  pure (prop <> mconcat rest)

blockConstraints :: Stmt () -> Gen LExprProp
blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
blockConstraints (SCall f xs) = error "SCall"
blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement
blockConstraints (lhs :*=: rhs) = do
  -- Add an apply constraint to the locus that was modified
  -- and unchanged constraints to the others

  -- create the apply expression
  let locus = toLocus lhs
  let apply_expr = LApply (Current locus) ("x" :=> (convertApplyExpr "x" rhs))
  -- For BellPair, all we have to do is inverse the locus TODO: actually get all the variables in the environment
  -- read the env (contains the function inputs and outputs, needed for the unchanged prop)
  env <- ask
  -- add the unchanged properties
  -- let unchanged = enumerateUnchanged (allBindings env) locus
  Prop ([Unchanged (Step (Current (invLocus locus)))] ++ [PointsTo (Step (Current locus)) apply_expr])
blockConstraints (SDafny _) = pure mempty
blockConstraints (SIf cond part body) = undefined

toLocus :: Partition -> Locus
toLocus (Partition xs) = Locus $ map convertRange xs

convertRange :: Qafny.Range -> Range
convertRange (Qafny.Range x start end) = Range x (convertExpr start) (convertExpr end)

-- |convertExpr converts a Qafny AST Expression to a SimpleExpr that can be used for symbolic execution
convertExpr :: Exp () -> SimpleExpr
convertExpr (ENum i) = Lit i
convertExpr (EVar x) = Var x
convertExpr (EOp2 OAdd x y) = Add (convertExpr x) (convertExpr y)

-- |convertApplyExpr converts a Qafny AST Expression used in an apply statement "*=" 
--  to a lambda that can be used in the LApply constructor.
--  where:
--    name of type `String` is the name to use for Hadamard gates and similar.
--    expression of type `Exp ()` is the expression tree to convert
--  return of type `SimpleExpr`
convertApplyExpr :: String -> Exp () -> SimpleExpr
convertApplyExpr name EHad = Hadamard name

