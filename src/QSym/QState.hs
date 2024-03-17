module QSym.QState
  (QState (..)
  ,atPosi
  ,update
  ,QEnv
  ,atVar
  ,envUpdate
  )
  where

import QSym.Utils

newtype QState a = QState (Posi -> a)

atPosi :: QState a -> Posi -> a
atPosi (QState f) = f

update :: QState a -> Posi -> a -> QState a
update st i new = QState $
  \j ->
    if j == i
    then new
    else atPosi st j

newtype QEnv a = QEnv (Var -> a)

atVar :: QEnv a -> Var -> a
atVar (QEnv f) = f

envUpdate :: QEnv a -> Var -> a -> QEnv a
envUpdate env i new = QEnv $
  \j ->
    if j == i
    then new
    else atVar env j

