{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FunctionalDependencies #-}

module QSym.Logic.IR
  (Nat (..)
  -- ,IxBounds (..)
  -- ,Ixs (..)
  ,Sum
  ,pattern Sum
  ,mkSum

  ,realToSMT
  ,bitVecToSMT

  ,control
  ,Controlled
  ,pattern Controlled
  ,cCondition
  ,cBody

  ,Expr
  ,var
  ,unMkVec

  ,EVec
  ,EBitVec
  ,EReal

  ,bvLit
  ,and'
  ,(.&&.)
  ,omega
  ,intLit
  ,overwriteBits
  ,getBitRange
  ,toBitVec
  ,fromBitVec
  ,fromInt
  ,(*.)
  ,(.==.)
  ,getBit
  ,invertBitVec
  ,getBitVec
  ,ampFactor
  ,getAmp
  ,getPhase
  ,mkVec
  )
  where

import Data.Ratio

import qualified QSym.Logic.SMT as SMT
import QSym.Logic.SMT (SMT, BitVector)

import QSym.Logic.Name

import Prettyprinter

data Nat = Z | S Nat

-- infixr :!
-- infixr :|

-- data IxBounds n where
--   IBZ :: IxBounds Z
--   (:!) :: Int -> IxBounds n -> IxBounds (S n)
--
-- data Ixs n where
--   IZ :: Ixs Z
--   (:|) :: Expr Int -> Ixs n -> Ixs (S n)

data Sum where
  MkSum ::
    [Int] ->
    (Expr EVec -> [Expr Int] -> Controlled EVec) ->
    Sum

-- | A unidirectional pattern. This allows us (outside this module)
-- to only match on Sum values, without being able to construct them using
-- the constructor.
-- To construct them outside the module, we must use the mkSum smart
-- constructor.
pattern Sum bounds f <- MkSum bounds f

mkSum ::
  [Int] ->
  (Expr EVec -> [Expr Int] -> Expr EVec) ->
  Sum
mkSum bounds f =
  MkSum bounds $ \vec ixs ->
    MkControlled (BoolLit True) (f vec ixs)

control :: (Expr EVec -> Expr Bool) -> Sum -> Sum
control predicate (MkSum bounds f) =
  MkSum bounds $ \vec ixs ->
    let Controlled fControlResult fBody = f vec ixs
    in
    MkControlled (predicate vec .&&. fControlResult) fBody

data Controlled a =
  MkControlled
    { cCondition :: Expr Bool
    , cBody :: Expr a
    }

pattern Controlled c b <- MkControlled c b

data EReal
data EBitVec
data EVec

unMkVec :: Expr b -> (Expr EReal, Expr EReal, Expr EBitVec)
unMkVec (MkVec x y z) = (x, y, z)

-- (Do not export the value constructors for Expr.)
data Expr b where
  Var :: String -> Expr a

  BitVecVar :: Int -> String -> Expr EBitVec

  MkVec ::
    Expr EReal ->   -- | Amplitude
    Expr EReal ->   -- | Phase
    Expr EBitVec -> -- | Bit vector
    Expr EVec

  GetAmp :: Expr EVec -> Expr EReal
  GetPhase :: Expr EVec -> Expr EReal
  GetBitVec :: Expr EVec -> Expr EBitVec

    -- | (1/sqrt(2))^n
  AmpFactor :: Int -> Expr EReal

  -- Bit vectors --
  GetBit :: Expr EBitVec -> Expr Int -> Expr EBitVec
  OverwriteBits :: Expr EBitVec -> Int -> Expr EBitVec -> Expr EBitVec
  GetBitRange :: Expr EBitVec -> Int -> Int -> Expr EBitVec
  InvertBitVec :: Expr EBitVec -> Expr EBitVec

  ToBitVec :: Int -> Expr Int -> Expr EBitVec
  FromBitVec :: Expr EBitVec -> Expr Int

  -- Integers --
  FromInt :: Expr Int -> Expr EReal
  IntLit :: Int -> Expr Int
  Omega :: Expr Int -> Expr Int -> Expr EReal

  -- Scalars --
  ScalarMult :: Expr EReal -> Expr EVec -> Expr EVec

  -- Misc arithmetic --
  Add :: Expr b -> Expr b -> Expr b
  Sub :: Expr b -> Expr b -> Expr b
  Mul :: Expr b -> Expr b -> Expr b
  Div :: Expr b -> Expr b -> Expr b

  -- Booleans --
  Equal :: Expr b -> Expr b -> Expr Bool
  BoolLit :: Bool -> Expr Bool
  And :: [Expr Bool] -> Expr Bool
  Or :: [Expr Bool] -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  -- deriving (Show)

var :: String -> Expr a
var = Var

(.==.) :: Expr a -> Expr a -> Expr Bool
(.==.) = Equal

instance Num (Expr EReal) where
  (+) = Add
  (-) = Sub
  (*) = mul
  abs = error "Expr Int: abs"
  signum = error "Expr Int: signum"

  fromInteger = FromInt . IntLit . fromInteger

instance Fractional (Expr EReal) where
  (/) = Div
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

instance Num (Expr Int) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = error "Expr Int: abs"
  signum = error "Expr Int: signum"

  fromInteger = IntLit . fromInteger

bvLit :: Int -> Int -> Expr EBitVec
bvLit n = ToBitVec n . IntLit

mul :: Expr EReal -> Expr EReal -> Expr EReal
mul (AmpFactor a) (AmpFactor b) = AmpFactor (a + b)
mul a (AmpFactor b) = mul (AmpFactor b) a
mul a (Mul b c) = mul (mul a b) c
mul (Mul a b) c = mul (mul a b) c
mul a b = Mul a b

and' :: [Expr Bool] -> Expr Bool
and' xs0 =
  case go xs0 of
    Left e -> e
    Right [] -> BoolLit True
    Right es -> And es
  where
    go :: [Expr Bool] -> Either (Expr Bool) [Expr Bool]
    go (And xs : rest) = go (xs ++ rest)
    go (BoolLit False : _) = Left $ BoolLit False
    go (BoolLit True : rest) = go rest
    go (b : bs) = do
      rs <- go bs
      pure (b : rs)

(.&&.) :: Expr Bool -> Expr Bool -> Expr Bool
x .&&. y = and' [x, y]

omega :: Expr Int -> Expr Int -> Expr EReal
omega = Omega

intLit :: Int -> Expr Int
intLit = IntLit

overwriteBits :: Expr EBitVec -> Int -> Expr EBitVec -> Expr EBitVec
overwriteBits = OverwriteBits

getBitRange :: Expr EBitVec -> Int -> Int -> Expr EBitVec
getBitRange = GetBitRange

toBitVec :: Int -> Expr Int -> Expr EBitVec
toBitVec = ToBitVec

fromBitVec :: Expr EBitVec -> Expr Int
fromBitVec = FromBitVec

fromInt :: Expr Int -> Expr EReal
fromInt = FromInt

(*.) :: Expr EReal -> Expr EVec -> Expr EVec
(*.) = ScalarMult

getBit = GetBit
invertBitVec = InvertBitVec
getBitVec = GetBitVec
ampFactor = AmpFactor
getAmp = GetAmp
getPhase = GetPhase
mkVec = MkVec

intToSMT :: Expr Int -> SMT Name Int
intToSMT (Var x) = SMT.var x
intToSMT (IntLit i) = SMT.int i
intToSMT (Add x y) = SMT.add (intToSMT x) (intToSMT y)
intToSMT (Sub x y) = SMT.sub (intToSMT x) (intToSMT y)
intToSMT (Mul x y) = SMT.mul (intToSMT x) (intToSMT y)
intToSMT (Div x y) = SMT.div (intToSMT x) (intToSMT y)
intToSMT (FromBitVec x) = SMT.bv2nat $ bitVecToSMT x

realToSMT :: Expr EReal -> SMT Name Int
realToSMT (Var x) = SMT.var x
realToSMT (FromInt i) = intToSMT i
realToSMT (Add x y) = SMT.add (realToSMT x) (realToSMT y)
realToSMT (Sub x y) = SMT.sub (realToSMT x) (realToSMT y)
realToSMT (Mul x y) = SMT.mul (realToSMT x) (realToSMT y)
realToSMT (Div x y) = SMT.div (realToSMT x) (realToSMT y)
realToSMT (Omega x y) = SMT.omega (intToSMT x) (intToSMT y)
realToSMT (AmpFactor n) = SMT.pow SMT.invSqrt2 (SMT.int n)
realToSMT (GetAmp x) =
  case x of
    MkVec amp _ _ -> realToSMT amp
realToSMT (GetPhase x) =
  case x of
    MkVec _ phase _ -> realToSMT phase

bitVecToSMT :: Expr EBitVec -> BitVector Name
bitVecToSMT (BitVecVar size x) = SMT.int2bv size $ SMT.var x
bitVecToSMT (GetBitVec x) =
  case x of
    MkVec _ _ bitVec -> bitVecToSMT bitVec

bitVecToSMT (OverwriteBits bv pos newPart) =
  SMT.overwriteBits (bitVecToSMT bv) (SMT.bvPosition pos) (bitVecToSMT newPart)

vecToSMT :: Expr EVec -> BitVector Name
vecToSMT (MkVec _ _ x) = bitVecToSMT x

