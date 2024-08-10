module QSym.Logic.Gen
  where

import Qafny.Syntax.AST hiding (Range (..), Block)
import qualified Qafny.Syntax.AST as Qafny
import Qafny.Syntax.Subst

import QSym.Logic.SMT
import QSym.Logic.Syntax

import Data.String
import Control.Monad.Reader

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
  }

getOtherInputs :: [Int] -> Gen [Int]
getOtherInputs usedInputs = do
  env <- ask

  let allInputs = zipWith const [0..] $ envInputs env

  pure $ filter (`notElem` usedInputs) allInputs

-- |allBindings returns a list of both the inputs and the outputs from the environment
allBindings :: Env -> Bindings ()
allBindings env = envInputs env ++ envOutputs env

buildEnv :: Int -> QMethod () -> Env
buildEnv bitSize qm = Env (qmInputs qm) (qmOutputs qm) bitSize

runGen :: Gen a -> Env -> a
runGen (Gen g) = runReader g

getSteppedVar :: Var -> [Name] -> [Stepped Var]
getSteppedVar x [] = []
getSteppedVar x (VarName y:ys)
  | getSteppedName y == x = y : getSteppedVar x ys
getSteppedVar x (_:ys) = getSteppedVar x ys

