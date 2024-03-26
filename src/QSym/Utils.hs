module QSym.Utils
  where

import Test.QuickCheck (Gen, choose, Property, forAll, chooseAny, (===))
import Data.Bits
import Data.Word

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

newtype RzValue = RzValue (Int -> Bool)

(!) :: RzValue -> Int -> Bool
RzValue f ! i = f i

-- indexRzValue :: RzValue -> Int -> Bool
-- indexRzValue (RzValue f) = f

data Value
  = NVal Bool RzValue
  | QVal RzValue RzValue

allFalse :: RzValue
allFalse = RzValue $ \_ -> False

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

