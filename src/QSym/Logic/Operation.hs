module QSym.Logic.Operation
  (Transform
  ,Accessor (..)
  ,mkIdentityAccessor

  ,Operation
  ,mkOperation
  ,runOperation
  ,extendAccessor
  )
  where

import QSym.Logic.SMT
import QSym.Logic.Name
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
      , opTransform :: Memory -> Accessor -> Transform
      }

runOperation :: Memory -> (Name -> Name) -> Operation -> (Memory, SMT Name Bool)
runOperation mem updateName op = (mem', generatedSMT)
  where
    newDims = EN (opAddedDims op)
    mem' = extendMemory mem newDims updateName

    generatedSMT =
      forEach mem' $ \ixs ->
        let transform = opTransform op mem (mkIdentityAccessor mem)
        in
        transform mem' ixs

mkOperation :: [Int] -> (Accessor -> Transform) -> Operation
mkOperation addedDims transform =
  Operation
    { opAddedDims = addedDims
    , opTransform = \_mem accessor -> transform accessor
    }

extendAccessor :: Operation -> (Memory -> Accessor) -> Operation
extendAccessor op newAccessorF =
  op
    { opTransform = \mem origAccessor -> opTransform op mem (origAccessor <> newAccessorF mem)
    }

reshape :: MemType -> [SMT Name Int] -> [SMT Name Int]
reshape newMemType ixs
  | oldDim > newDim = error $ "reshape: oldDim (" ++ show oldDim ++ ") > (" ++ show newDim ++ ")"
  | otherwise = replicate (newDim - oldDim) (int 0) ++ ixs
  where
    oldDim = length ixs
    newDim = memTypeSize newMemType

