{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module QSym.Logic.Memory
  -- (MemType (..)
  -- ,Memory
  -- ,mkMemory
  -- ,forEach
  -- ,setIndex1
  -- ,setIndex2
  -- ,setIndex3
  -- )
  where

import QSym.Logic.Gen
import QSym.Logic.SMT

import GHC.Stack

newtype MemType = EN [Int] -- Contains the dimensions. If the list is size n, then it represents the type EN(n)
  deriving (Show, Eq, Ord, Semigroup)

data Memory =
  Memory
    { memType :: MemType
    , memBitVecSize :: Int
    , memAmpName :: Name
    , memPhaseName :: Name
    , memBitVecName :: Name
    }
  deriving (Show)

data MemEntry =
  MemEntry
    { memEntryAmp :: SMT Name Int
    , memEntryPhase :: SMT Name Int
    , memEntryBitVec :: BitVector Name
    }

memTypeSize :: MemType -> Int
memTypeSize (EN xs) = length xs

mkMemory :: MemType -> Int -> Name -> Name -> Name -> Memory
mkMemory = Memory

-- | Extend with more dimensions of given size. We also update the names.
extendMemory :: Memory -> [Int] -> (Name -> Name) -> Memory
extendMemory mem newDims updateName =
  mem
    { memType = memType mem <> EN newDims
    , memAmpName = updateName $ memAmpName mem
    , memPhaseName = updateName $ memPhaseName mem
    , memBitVecName = updateName $ memBitVecName mem
    }

mkBounds :: MemType -> [String] -> SMT Name Bool
mkBounds (EN uppers) vs =
    and' (map mkLowerBound vs ++ zipWith mkUpperBound uppers vs)
  where
    mkLowerBound v = gte (var v) (int 0)
    mkUpperBound upper v = lt (var v) (int upper)

-- TODO: Make sure that these names are always appropriately fresh
mkIndexVars :: MemType -> SomeIxs Name Int Int
mkIndexVars (EN xs) = go xs names
  where
    go :: [Int] -> [String] -> SomeIxs Name Int Int
    go [_] (name:_) = SomeIxs (OneIx (var name))
    go (_:restDims) (name:restNames) =
      case go restDims restNames of
        SomeIxs r -> SomeIxs (ConsIx (var name) r)

    baseNames = ["i", "j", "k"]

    names = baseNames ++ map extraNameToString extraNames

    extraNameToString (str, i) = str ++ show i

    extraNames :: [(String, Int)]
    extraNames = zip baseNames [0..] ++ map (fmap (+1)) extraNames

indexMemory :: Memory -> Ixs Name (Array Int a) Int Int -> MemEntry
indexMemory mem ixs
  | length (ixsToList ixs) /= memTypeSize (memType mem) = error $ "indexMemory: Wrong number of dimensions. Memory has " ++ show (memTypeSize (memType mem)) ++ ", expected " ++ show (length (ixsToList ixs))
  | otherwise =
      -- TODO: Find a nicer way
      case retagIxs_unsafe ixs :: SomeIxs Name Int (Array Int Int) of
        SomeIxs bitVecIxs ->
          MemEntry
            { memEntryAmp = selects ampArr ixs
            , memEntryPhase = selects phaseArr ixs
            , memEntryBitVec = mkBitVectorOfSize (memBitVecSize mem) $ selects bitVecArr bitVecIxs
            }
      where
        ampArr = symbol (memAmpName mem)
        phaseArr = symbol (memPhaseName mem)
        bitVecArr = symbol (memBitVecName mem)

indexMemoryByList :: Memory -> [SMT Name Int] -> MemEntry
indexMemoryByList mem ixs =
    case ixsFromList ixs of
      SomeIxs is -> indexMemory mem is
  where
    ixsFromList :: [SMT Name Int] -> SomeIxs Name Int Int
    ixsFromList [i] = SomeIxs (OneIx i)
    ixsFromList (i:is) = 
      case ixsFromList is of
        SomeIxs is' -> SomeIxs (ConsIx i is')

forEach :: Memory -> ([SMT Name Int] -> SMT Name Bool) -> SMT Name Bool
forEach mem f =
  case mkIndexVars (memType mem) of
    SomeIxs ixs ->
      let namesList = ixsToNames_unsafe ixs
      in
      forAll (map (, "Int") namesList) $
        implies (mkBounds (memType mem) namesList)
          (f (ixsToList ixs))


-- forEach :: Memory -> (MemEntry -> SMT Name Bool) -> SMT Name Bool
-- forEach mem f =
--   case mkIndexVars (memType mem) of
--     SomeIxs ixs ->
--       let namesList = ixsToNames_unsafe ixs
--       in
--       forAll (map (, "Int") namesList) $
--         implies (mkBounds (memType mem) namesList)
--           (f (indexMemory mem ixs))

-- | 1 dimensions
setIndex1 :: Memory -> (SMT Name Int -> MemEntry) -> SMT Name Bool
setIndex1 mem0 = undefined
  where
    mem = ensureMemType mem0 1 --(EN 1)

-- | 2 dimensions
setIndex2 :: Memory -> (SMT Name Int -> SMT Name Int -> MemEntry) -> SMT Name Bool
setIndex2 mem0 = undefined
  where
    mem = ensureMemType mem0 2 --(EN 2)

-- | 3 dimensions
setIndex3 :: Memory -> (SMT Name Int -> SMT Name Int -> SMT Name Int -> MemEntry) -> SMT Name Bool
setIndex3 mem0 = undefined
  where
    mem = ensureMemType mem0 3 --(EN 3)

-- data Nat = Z | S Nat
--
-- infixr :|
-- data Tuple n a where
--   TupleOne :: a -> Tuple ('S 'Z) a
--   (:|) :: a -> Tuple n a -> Tuple ('S n) a
--
-- type Dim1 = 'S 'Z
-- type Dim2 = 'S Dim1
-- type Dim3 = 'S Dim2
-- type Dim4 = 'S Dim3

type Modifier = Memory -> [SMT Name Int] -> MemEntry -> MemEntry
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

-- extendAccessor :: Accessor -> (Memory -> Accessor) -> Accessor
-- extendAccessor acc f = Accessor $ \ix k ->
--   acc ix $

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

setToMemEntry :: Memory -> [SMT Name Int] -> MemEntry -> SMT Name Bool
setToMemEntry mem ixs entry =
  and'
    [ eq (selectWithList (symbol (memAmpName mem)) ixs)
         (memEntryAmp entry)

    , eq (selectWithList (symbol (memPhaseName mem)) ixs)
         (memEntryPhase entry)

    , eq (selectWithList (symbol (memBitVecName mem)) ixs)
         (bvSMT (memEntryBitVec entry))
    ]

-- runOperation :: Memory -> (Name -> Name) -> Operation -> SMT Name Bool
-- runOperation mem updateName op =
--   let mem' = extendMemory mem (opAddedDims op) updateName
--   in
--   forEach mem' $ \ixs ->
--     setToMemEntry mem' ixs (opTransform op mem ixs)

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
              { memEntryAmp = 1 -- ?
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
  op
    { opTransform = \mem f -> opTransform op mem (\mem' -> f mem <> controlledAccessor gatePosition p mem) -- INVARIANT: mem and mem' should be the same
    }
    

-- controlled :: Int -> Operation -> Operation
-- controlled gatePosition0 = changeModifier controlModifier
--   where
--     controlModifier = undefined
--   -- let gatePosition = bvPosition gatePosition0
--   -- in
--   -- Operation
--   --   { opAddedDims = opAddedDims op
--   --   , opTransform = \mem ixs ->
--   --       undefined
--   --   }

-- -- Hadamard:
-- --   setIndex2 (extendMemory mem nextNames)
-- --      $ \j k ->
-- --          let oldEntry = indexMemoryByList mem [j]
-- --              oldBvEntry = memEntryBitVec oldEntry
-- --              bit = bv2nat (bvGetRange oldBvEntry gatePosition gatePosition)
-- --          in
-- --          MemEntry
-- --            { memEntryAmp = 1 -- ?
-- --            , memEntryPhase = omega (bit * k)
-- --            , memEntryBitVec = overwriteBits oldBvEntry gatePosition (nat2bv k)
-- --            }
--
-- -- NOT:
-- --   setIndex1 (extendMemory mem nextNames)
-- --      $ \j ->
-- --          let oldEntry = indexMemoryByList mem [j]
-- --              oldBvEntry = memEntryBitVec oldEntry
-- --              bit = bv2nat (bvGetRange oldBvEntry gatePosition gatePosition)
-- --          in
-- --          MemEntry
-- --            { memEntryAmp = memEntryAmp mem
-- --            , memEntryPhase = memEntryPhase mem
-- --            , memEntryBitVec = overwriteBits oldBvEntry gatePosition (invertBitVec bit)
-- --            }
--
-- -- IF p, given an f:
-- --   setIndex1 (extendMemory mem nextNames)
-- --      $ \j ->
-- --          let oldEntry = indexMemoryByList mem [j]
-- --              oldBvEntry = memEntryBitVec oldEntry
-- --              bits = bv2nat (bvGetRange oldBvEntry gateInputStart gateInputEnd)
-- --          in
-- --          ifThenElse (p bits)
-- --            ()
-- --            (reshape mem (gateDimensions f))

ensureMemType :: HasCallStack => Memory -> Int -> Memory
ensureMemType mem size = ensureType mem (memType mem) size

ensureType :: HasCallStack => a -> MemType -> Int -> a
ensureType r mty@(EN ty) size
  | length ty >= size = r -- TODO: Is '>=' right?
  | otherwise = error $ "Expected EN(" <> show (length ty) <> "), got EN(" <> show size <> ")"

