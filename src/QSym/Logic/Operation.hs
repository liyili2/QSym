module QSym.Logic.Operation
  (Transform
  ,Accessor (..)
  ,mkIdentityAccessor

  ,Operation
  ,mkOperation
  ,extendAccessor
  )
  where

import QSym.Logic.SMT
import QSym.Logic.Gen
import QSym.Logic.Memory

type Transform = Memory -> [SMT Name Int] -> SMT Name Bool

-- | This mediates access to the previous memory state
newtype Accessor = Accessor { runAccessor :: Memory -> [SMT Name Int] -> [SMT Name Int] -> (MemEntry -> SMT Name Bool) -> SMT Name Bool }

instance Semigroup Accessor where
  Accessor p <> Accessor q =
    Accessor $ \mem' ix newIx k ->
      p mem' ix newIx (\_ -> q mem' ix newIx k)

mkIdentityAccessor :: Memory -> Accessor
mkIdentityAccessor mem = Accessor $ \_mem' ix _newIx k ->
  k (indexMemoryByList mem ix)

data Operation
  = Operation
      { opAddedDims :: [Int]
      , opTransform :: Memory -> (Memory -> Accessor) -> Transform
      }

mkOperation :: [Int] -> (Accessor -> Transform) -> Operation
mkOperation addedDims transform =
  Operation
    { opAddedDims = addedDims
    , opTransform = \mem f -> transform (f mem)
    }

extendAccessor :: Operation -> (Memory -> Accessor) -> Operation
extendAccessor op newAccessorF =
  op
    { opTransform = \mem f -> opTransform op mem (\mem' -> f mem <> newAccessorF mem) -- INVARIANT: mem and mem' should be the same
    }

reshape :: MemType -> [SMT Name Int] -> [SMT Name Int]
reshape newMemType ixs
  | oldDim > newDim = error $ "reshape: oldDim (" ++ show oldDim ++ ") > (" ++ show newDim ++ ")"
  | otherwise = replicate (newDim - oldDim) (int 0) ++ ixs
  where
    oldDim = length ixs
    newDim = memTypeSize newMemType

