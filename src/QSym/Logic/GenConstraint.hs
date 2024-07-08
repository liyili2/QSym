module QSym.Logic.GenConstraint
  (astConstraints
  )
  where

import QSym.Logic.Syntax

import Qafny.Syntax.AST hiding (Range (..))
import qualified Qafny.Syntax.AST as Qafny

import Data.Sum

import Control.Monad.Reader

newtype Gen a = Gen { getGen :: Reader Env a }
  deriving (Functor, Applicative, Monad, MonadReader Env)

data Env =
  Env
  { envInputs :: Bindings ()
  , envOutputs :: Bindings ()
  }

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
blockConstraints (lhs :*=: rhs) =
  -- Apply
  undefined
blockConstraints (SDafny _) = pure mempty
blockConstraints (SIf cond part body) = undefined

toLocus :: Partition -> Locus
toLocus (Partition xs) = Locus $ map convertRange xs

convertRange :: Qafny.Range -> Range
convertRange (Qafny.Range x start end) = Range x (convertExpr start) (convertExpr end)

convertExpr :: Exp () -> SimpleExpr
convertExpr (ENum i) = Lit i
convertExpr (EVar x) = Var x
convertExpr (EOp2 OAdd x y) = Add (convertExpr x) (convertExpr y)

