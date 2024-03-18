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

atPosi :: QState a -> Posi -> a
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
    varEquiv st st' x (atVar env x)

varEquiv :: QState Value -> QState Value -> Var -> Int -> Property
varEquiv st st' var n =
  n /= 0 ==>
    let n' = n - 1
        p = Posi var n'
    in
    valueEqualUpToCutOff (atPosi st p) (atPosi st' p) === True
      .&.
    varEquiv st st' var n'

equalUpToCutoff :: RzValue -> RzValue -> Bool
equalUpToCutoff f g = all go [0..bitCheckCutoff]
  where
    go x = (f ! x) == (g ! x)

valueEqualUpToCutOff :: Value -> Value -> Bool
valueEqualUpToCutOff (NVal b v) (NVal b' v') = b == b' && equalUpToCutoff v v'
valueEqualUpToCutOff (QVal v1 v2) (QVal v1' v2') = equalUpToCutoff v1 v1' && equalUpToCutoff v2 v2'
valueEqualUpToCutOff _ _ = False

