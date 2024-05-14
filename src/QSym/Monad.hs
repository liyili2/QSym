module QSym.Monad
  -- (QSym
  -- ,execQSym
  -- ,atPosi
  -- ,update
  -- ,atVar
  -- ,emptyState
  -- ,mkState
  -- -- ,mkEnv
  -- )
  where

-- import QSym.QState (QEnv', QState', emptyState, mkState)
import qualified QSym.QState as QS
-- import QSym.QState (QEnv, mkEnv)

import QSym.Utils
import QSym.Syntax

import Control.Monad.Reader
import Control.Monad.State

import Numeric.Natural
import Test.QuickCheck
import Data.Bits

newtype QSym a = QSym (ReaderT QEnv' (State QState') a)
  deriving (Functor, Applicative, Monad, MonadReader QEnv')

-- Specialized to () so that we know we're forgetting the "result"
execQSym :: QSym () -> QEnv' -> QState' -> QState'
execQSym (QSym m) env st =
  execState (runReaderT m env) st

stateGet :: Var -> QSym Value
stateGet x =
  stateGet' <$> QSym get <*> pure x

update :: Var -> Value -> QSym ()
update i new =
  QSym $ modify $ \st -> update' st i new

envGet :: Var -> QSym Int
envGet x = envGet' <$> QSym ask <*> pure x

-- Bit count usually won't change, except Nor -> Phi
-- (Starts at Nor type)
-- Initial bit count is determined by the environment size
data RzValue = RzValue
  { rzBitCount :: Int
  , rzNatural :: Natural
  }
  deriving (Show) --(Eq, Ord, Num, Bits, Show)

instance Eq RzValue -- TODO: Implement
instance Ord RzValue -- TODO: Implement
instance Enum RzValue -- TODO: Implement
instance Num RzValue -- TODO: Implement
instance Real RzValue -- TODO: Implement
instance Integral RzValue -- TODO: Implement

rzValueToBools :: RzValue -> [Bool]
rzValueToBools val@(RzValue count _) =
  map (\i -> val ! i) [0..count-1]

boolsToRzValue :: [Bool] -> RzValue
boolsToRzValue xs0 = go 0 xs0
  where
    len = length xs0

    go i [] = RzValue len 0
    go i (True:xs) =
      let rest = go (i+1) xs
      in
      rzSetBit rest i True
    go i (False:xs) = go (i+1) xs

-- TODO: Use a fast implementation
rotate :: Int -> RzValue -> RzValue
rotate i = boolsToRzValue . go i . rzValueToBools
  where
    go :: Int -> [a] -> [a]
    go = drop <> take

rzValue :: MonadReader (QEnv a) m => Natural -> m RzValue
rzValue val = do
  QEnv xs <- ask
  pure (RzValue (length xs) val)

(!) :: RzValue -> Int -> Bool
(!) (RzValue _ v) i = testBit v i

rzSetBit :: RzValue -> Int -> Bool -> RzValue
rzSetBit (RzValue sz f) i b = RzValue sz $
  if b
  then setBit f i
  else clearBit f i

nOnes :: Int -> QSym RzValue
nOnes n = rzValue ((1 `shiftL` n) - 1)

-- Apply the function to bits below the given index
mapBitsBelow :: Int -> RzValue -> (Int -> Bool) -> RzValue
mapBitsBelow i0 v0 f = go i0 v0
  where
    go i v
      | i < 0     = v
      | otherwise = rzSetBit (go (i - 1) v) i (f i)

bool2RzValue :: MonadReader (QEnv a) m => Bool -> m RzValue
bool2RzValue False = rzValue 0
bool2RzValue True = rzValue 1

data Value
  = NVal RzValue RzValue
  | QVal RzValue RzValue
  
allFalse :: MonadReader (QEnv a) m => m RzValue
allFalse = rzValue 0

-- basisVal :: Bool -> Value
-- basisVal b = NVal b allFalse

newtype QEnv a = QEnv [(Var, a)] --(Var -> a)

type QEnv' = QEnv Int

envGet' :: QEnv a -> Var -> a
envGet' (QEnv xs) v =
  let Just r = lookup v xs
  in
  r

qenvSize :: QEnv a -> Int
qenvSize (QEnv xs) = length xs

newtype QState a = QState (Var -> a)

type QState' = QState Value

stateGet' :: QState a -> Var -> a
stateGet' (QState f) = f

update' :: QState a -> Var -> a -> QState a
update' st i new = QState $
  \j ->
    if j == i
    then new
    else stateGet' st j

-- updateVar' :: QState Value -> Posi -> Bvector -> QState Value
-- updateVar' st x vec =
--   case unconsBvector vec of
--     Nothing -> st
--     Just (b, vec') ->
--       update (updateVar' st (nextPos x) vec') x (basisVal b)

-- updateVar :: QState Value -> Var -> Bvector -> QState Value
-- updateVar st x = updateVar' st (Posi x 0)

emptyState :: QEnv a -> QState Value
emptyState env = QState $ \_ -> liftA2 NVal allFalse allFalse env

mkState :: QEnv a -> [(Var, Value)] -> QState Value
mkState env [] = emptyState env
mkState env ((p, v):rest) = update' (mkState env rest) p v

-- stateFromVars :: [(Var, Bvector)] -> QState Value
-- stateFromVars [] = emptyState
-- stateFromVars ((x, vec) : rest) =
--   updateVar (stateFromVars rest) x vec

-- envUpdate :: QEnv a -> Var -> a -> QEnv a
-- envUpdate env i new = QEnv $
--   \j ->
--     if j == i
--     then new
--     else atVar env j

-- stEquiv :: [Var] -> QEnv Int -> QState Value -> QState Value -> Property
-- stEquiv vars env st st' =
--   forAll (elements vars) $ \x ->
--     counterexample (show (showVar st x, showVar st' x)) $
--     varEquiv st st' x (atVar env x)

-- showVar :: QState Value -> Var -> String
-- showVar st x = go 0
--   where
--     go n | n >= 5 = ""
--     go n =
--       showValue (atPosi st (Posi x n)) ++ ", " ++ go (n + 1)

-- varEquiv :: QState Value -> QState Value -> Var -> Int -> Property
-- varEquiv st st' var n =
--   n /= 0 ==>
--     let n' = n - 1
--         p = Posi var n'
--     in
--     (counterexample (show (p, showValue (atPosi st p), showValue (atPosi st' p)))
--       (valueEqualUpToCutOff (atPosi st p) (atPosi st' p)))
--       .&.
--     varEquiv st st' var n'

-- showValue :: Value -> String
-- showValue (NVal b v) =
--   "NVal " ++ show b ++ " " ++ show (toInt v)
-- showValue (QVal v1 v2) =
--   "QVal " ++ show (toInt v1) ++ show (toInt v2)

-- upToCutoff :: RzValue -> [Bool]
-- upToCutoff = zipWith (flip (!)) [0..bitCheckCutoff] . repeat

-- toInt :: RzValue -> Int
-- toInt = go 0 . upToCutoff
--   where
--     go i [] = 0
--     go i (False : xs) =           go (i+1) xs
--     go i (True  : xs) = (2 ^ i) + go (i+1) xs

-- equalUpToCutoff :: RzValue -> RzValue -> Bool
-- equalUpToCutoff f g = all go [0..bitCheckCutoff]
--   where
--     go x = (f ! x) == (g ! x)

-- valueEqualUpToCutOff :: Value -> Value -> Property
-- valueEqualUpToCutOff (NVal b v) (NVal b' v') =
--   b === b' .&&. upToCutoff v === upToCutoff v'
-- valueEqualUpToCutOff (QVal v1 v2) (QVal v1' v2') = upToCutoff v1 == upToCutoff v1' .&&. upToCutoff v2 === upToCutoff v2'
-- valueEqualUpToCutOff _ _ = counterexample "QVal /= NVal" (False === True)
--
-- mkEnv :: Show a => [(Var, a)] -> QEnv a
-- mkEnv xs = QEnv $ \x ->
--   case lookup x xs of
--     Nothing -> error $ "Cannot find " ++ show x ++ " in " ++ show xs
--     Just y -> y
--
