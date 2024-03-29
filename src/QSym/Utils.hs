module QSym.Utils
  (Var (..)
  ,xVar, yVar, zVar
  ,Posi (..)
  ,getPosiVar
  ,nextPos
  ,RzValue
  ,(!)
  ,rzSetBit
  ,nOnes
  ,mapBitsBelow
  ,bool2RzValue
  ,Value (..)
  ,basisVal
  ,allFalse
  ,Bvector
  ,genBvector
  ,unconsBvector
  ,bvector2Int
  ,int2Bvector
  ,bvectorIntTest
  ,intBvectorTest
  ,modBvector
  ,divBvector
  ,unconsBit
  )
  where

import Test.QuickCheck (Gen, choose, Property, forAll, chooseAny, (===))
import Data.Bits
import Data.Word
import Numeric.Natural

newtype Var = Var Int
  deriving (Show, Eq, Ord)

xVar, yVar, zVar :: Var
xVar = Var 0
yVar = Var 1
zVar = Var 2

data Posi = Posi Var Int
  deriving (Show, Eq)

getPosiVar :: Posi -> Var
getPosiVar (Posi x _) = x

nextPos :: Posi -> Posi
nextPos (Posi x i) = Posi x (i + 1)

newtype RzValue = RzValue Natural
  deriving (Eq, Ord, Num, Bits, Show)

(!) :: RzValue -> Int -> Bool
(!) (RzValue v) i = testBit v i

rzSetBit :: RzValue -> Int -> Bool -> RzValue
rzSetBit (RzValue f) i b = RzValue $
  if b
  then setBit f i
  else clearBit f i

nOnes :: Int -> RzValue
nOnes n = (1 `shiftL` n) - 1

-- Apply the function to bits below the given index
mapBitsBelow :: Int -> RzValue -> (Int -> Bool) -> RzValue
mapBitsBelow i0 v0 f = go i0 v0
  where
    go i v
      | i < 0     = v
      | otherwise = rzSetBit (go (i - 1) v) i (f i)

bool2RzValue :: Bool -> RzValue
bool2RzValue False = 0
bool2RzValue True = 1

data Value
  = NVal Bool RzValue
  | QVal RzValue RzValue

allFalse :: RzValue
allFalse = RzValue 0

basisVal :: Bool -> Value
basisVal b = NVal b allFalse

data Bvector = Bvector Int -- Bit length
                       Word64 -- Actual bits
  deriving (Show, Eq, Ord)

genBvector :: Int -> Gen Bvector
genBvector maxSize =
  Bvector maxSize <$> choose (0, 2^maxSize - 1)

unconsBvector :: Bvector -> Maybe (Bool, Bvector)
unconsBvector (Bvector 0 _) = Nothing
unconsBvector (Bvector sz v) =
  Just $ fmap (Bvector (sz - 1)) (unconsBit v)

bvector2Int :: Bvector -> Word64
bvector2Int (Bvector _ i) = i

-- int2Bvector :: (FiniteBits a, Integral a) => a -> Bvector
int2Bvector :: Word64 -> Bvector
int2Bvector = \i -> Bvector (getMaxBitSet i) (fromIntegral i)
  where
    getMaxBitSet j = finiteBitSize j - countLeadingZeros j

bvectorIntTest :: Property
bvectorIntTest =
  forAll chooseAny $ \(x :: Word64) ->
  x === bvector2Int (int2Bvector x)

intBvectorTest :: Property
intBvectorTest =
  forAll (genBvector 63) $ \bv ->
  getActualBits bv === getActualBits (int2Bvector (bvector2Int bv))
  where
    getActualBits (Bvector _ b) = b

liftBinaryIntOp :: (Word64 -> Word64 -> Word64) -> Bvector -> Bvector -> Bvector
liftBinaryIntOp op vx vy =
  int2Bvector (op (bvector2Int vx) (bvector2Int vy))

modBvector :: Bvector -> Bvector -> Bvector
modBvector = liftBinaryIntOp mod

divBvector :: Bvector -> Bvector -> Bvector
divBvector = liftBinaryIntOp div

unconsBit :: (Num a, Bits a) => a -> (Bool, a)
unconsBit n =
  (n .&. 1 == 1
  ,n `shiftR` 1
  )

