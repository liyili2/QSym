module QSym.QState
  (QState (..)
  ,atPosi
  ,update
  ,QEnv (..)
  ,atVar
  ,envUpdate
  ,stEquiv
  ,emptyState
  ,mkState
  ,stateFromVars
  )
  where

import QSym.Utils
import Test.QuickCheck

bitCheckCutoff :: Int
bitCheckCutoff = 63

newtype QState a = QState (Posi -> a)

atPosi :: QState a -> Var -> a
atPosi (QState f) = f

update :: QState a -> Posi -> a -> QState a
update st i new = QState $
  \j ->
    if j == i
    then new
    else atPosi st j

updateVar' :: QState Value -> Posi -> Bvector -> QState Value
updateVar' st x vec =
  case unconsBvector vec of
    Nothing -> st
    Just (b, vec') ->
      update (updateVar' st (nextPos x) vec') x (basisVal b)

updateVar :: QState Value -> Var -> Bvector -> QState Value
updateVar st x = updateVar' st (Posi x 0)

emptyState :: QState Value
emptyState = QState $ \_ -> NVal False allFalse

mkState :: [(Posi, Value)] -> QState Value
mkState [] = emptyState
mkState ((p, v):rest) = update (mkState rest) p v

stateFromVars :: [(Var, Bvector)] -> QState Value
stateFromVars [] = emptyState
stateFromVars ((x, vec) : rest) =
  updateVar (stateFromVars rest) x vec

newtype QEnv a = QEnv (Var -> a)

atVar :: QEnv a -> Var -> a
atVar (QEnv f) = f

envUpdate :: QEnv a -> Var -> a -> QEnv a
envUpdate env i new = QEnv $
  \j ->
    if j == i
    then new
    else atVar env j

stEquiv :: [Var] -> QEnv Int -> QState Value -> QState Value -> Property
stEquiv vars env st st' =
  forAll (elements vars) $ \x ->
    counterexample (show (showVar st x, showVar st' x)) $
    varEquiv st st' x (atVar env x)

showVar :: QState Value -> Var -> String
showVar st x = go 0
  where
    go n | n >= 5 = ""
    go n =
      showValue (atPosi st (Posi x n)) ++ ", " ++ go (n + 1)

varEquiv :: QState Value -> QState Value -> Var -> Int -> Property
varEquiv st st' var n =
  n /= 0 ==>
    let n' = n - 1
        p = Posi var n'
    in
    (counterexample (show (p, showValue (atPosi st p), showValue (atPosi st' p)))
      (valueEqualUpToCutOff (atPosi st p) (atPosi st' p)))
      .&.
    varEquiv st st' var n'

showValue :: Value -> String
showValue (NVal b v) =
  "NVal " ++ show b ++ " " ++ show (toInt v)
showValue (QVal v1 v2) =
  "QVal " ++ show (toInt v1) ++ show (toInt v2)

upToCutoff :: RzValue -> [Bool]
upToCutoff = zipWith (flip (!)) [0..bitCheckCutoff] . repeat

toInt :: RzValue -> Int
toInt = go 0 . upToCutoff
  where
    go i [] = 0
    go i (False : xs) =           go (i+1) xs
    go i (True  : xs) = (2 ^ i) + go (i+1) xs

-- equalUpToCutoff :: RzValue -> RzValue -> Bool
-- equalUpToCutoff f g = all go [0..bitCheckCutoff]
--   where
--     go x = (f ! x) == (g ! x)

valueEqualUpToCutOff :: Value -> Value -> Property
valueEqualUpToCutOff (NVal b v) (NVal b' v') =
  b === b' .&&. upToCutoff v === upToCutoff v'
valueEqualUpToCutOff (QVal v1 v2) (QVal v1' v2') = upToCutoff v1 == upToCutoff v1' .&&. upToCutoff v2 === upToCutoff v2'
valueEqualUpToCutOff _ _ = counterexample "QVal /= NVal" (False === True)

