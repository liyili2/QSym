module QSym.QState
  (QState
  ,atPosi
  ,update
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

