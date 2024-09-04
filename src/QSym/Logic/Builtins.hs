module QSym.Logic.Builtins
  where

import QSym.Logic.SMT
import QSym.Logic.Gen
import QSym.Logic.Memory
import QSym.Logic.Operation

hadamard :: Int -> Operation
hadamard gatePosition0 =
  let gatePosition = bvPosition gatePosition0
      bitsAppliedTo = 1
  in
  mkOperation [2] $
    \accessor mem' [j, k] ->
        runAccessor accessor mem' [j] [j, k] $ \oldEntry ->
          let oldBvEntry = memEntryBitVec oldEntry
              bit = bv2nat (bvGetRange oldBvEntry gatePosition gatePosition)
          in
          setToMemEntry mem' [j, k] $
            MemEntry
              { memEntryAmp = memEntryAmp oldEntry -- TODO: Is this right?
              , memEntryPhase = omega (bit * k) (2 ^ bitsAppliedTo)
              , memEntryBitVec = overwriteBits oldBvEntry gatePosition (int2bv bitsAppliedTo k)
              }

notOp :: Int -> Operation
notOp gatePosition0 =
  let gatePosition = bvPosition gatePosition0
  in
  mkOperation [] $
    \accessor mem' [j] ->
        runAccessor accessor mem' [j] [j] $ \oldEntry ->
          let oldBvEntry = memEntryBitVec oldEntry
              bit = bv2nat (bvGetRange oldBvEntry gatePosition gatePosition)
          in
          setToMemEntry mem' [j] $
          MemEntry
            { memEntryAmp = memEntryAmp oldEntry
            , memEntryPhase = memEntryPhase oldEntry
            , memEntryBitVec = overwriteBits oldBvEntry gatePosition (invertBitVec (int2bv 1 bit))
            }

controlledAccessor :: Int -> (MemEntry -> SMT Name Bool) -> Memory -> Accessor
controlledAccessor gatePosition p mem = Accessor $ \mem' ix newIx k ->
  let entry = indexMemoryByList mem ix
  in
  ifThenElse (p entry)
    (k entry)
    (setToMemEntry mem' newIx entry)

controlled :: Int -> (MemEntry -> SMT Name Bool) -> Operation -> Operation
controlled gatePosition p op =
  extendAccessor op $ controlledAccessor gatePosition p

controlledNot :: Int -> Int -> Operation
controlledNot controlPosition notPosition =
  controlled controlPosition predicate (notOp notPosition)
  where
    predicate entry =
      eq (bvSMT (getBit (memEntryBitVec entry) (bvPosition controlPosition)))
         (bvSMT (bvLit 1 0x1))

