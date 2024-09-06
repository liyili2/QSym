{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module QSym.Logic.IR
  where

type family Ix a

-- type family Ix a
-- type family Coeff a
-- type family BitVec a
-- type family Vec a
-- type family Boolean a
-- type family IntTy a

data Nat = Z | S Nat

-- data MemEntry a =
--   MemEntry
--     { memEntryAmp :: Expr EReal
--     , memEntryPhase :: Expr EReal
--     , memEntryBitVec :: Expr EBitVec
--     }

infixr :!
infixr :|

data IxBounds n where
  IBZ :: IxBounds Z
  (:!) :: Int -> IxBounds n -> IxBounds (S n)

data Ixs n where
  IZ :: Ixs Z
  (:|) :: Expr Int -> Ixs n -> Ixs (S n)

data Sum where
  MkSum ::
    IxBounds n ->
    (Expr EVec -> Ixs n -> Controlled EVec) ->
    Sum

mkSum ::
  IxBounds n ->
  (Expr EVec -> Ixs n -> Expr EVec) ->
  Sum
mkSum bounds f =
  MkSum bounds $ \vec ixs ->
    Controlled (BoolLit True) (f vec ixs)

control :: (Expr EVec -> Expr Bool) -> Sum -> Sum
control predicate (MkSum bounds f) =
  MkSum bounds $ \vec ixs ->
    let Controlled fControlResult fBody = f vec ixs
    in
    Controlled (predicate vec .&&. fControlResult) fBody

data Controlled a =
  Controlled
    { cCondition :: Expr Bool
    , cBody :: Expr a
    }

data EReal
data EBitVec
data EVec

data Expr b where
  Var :: -- NOTE: Do not make this visible
    String -> Expr a

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
  Sqrt :: Expr b -> Expr b

  -- Booleans --
  Equal :: Expr b -> Expr b -> Expr Bool
  BoolLit :: Bool -> Expr Bool
  And :: [Expr Bool] -> Expr Bool
  Or :: [Expr Bool] -> Expr Bool
  Not :: Expr Bool -> Expr Bool

instance Num (Expr EReal) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger = FromInt . IntLit . fromInteger

instance Fractional (Expr EReal) where
  (/) = Div

instance Floating (Expr EReal) where
  sqrt = Sqrt

instance Num (Expr Int) where
  (+) = Add
  (-) = Sub
  (*) = Mul

  fromInteger = IntLit . fromInteger

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

hadamard :: Int -> Sum
hadamard whichQubit =
  let qubitsAppliedTo = 1
  in
  mkSum (2 :! IBZ)         -- Upper bounds for additional indices for summation
    $ \oldVec (j :| IZ) -> -- Additional indices for summation
        let bit = FromBitVec (GetBit (GetBitVec oldVec) j)
        in
        MkVec
          (AmpFactor 1 * GetAmp oldVec)
          (Omega (bit * j) (2 ^ qubitsAppliedTo))
          (OverwriteBits (GetBitVec oldVec)
                         whichQubit
                         (ToBitVec qubitsAppliedTo j))



-- control :: Expr Bool -> Expr EVec -> Expr EVec
-- control = Control
--
-- omega :: Expr (IntTy a) -> Expr (IntTy a) -> Expr (Coeff a)
-- omega = Omega
--
-- intLit :: Int -> Expr (IntTy a)
-- intLit = IntLit
--
-- overwriteBits :: Expr (BitVec a) -> Int -> Expr (BitVec a) -> Expr (BitVec a)
-- overwriteBits = OverwriteBits
--
-- getBitRange :: Expr (BitVec a) -> Int -> Int -> Expr (BitVec a)
-- getBitRange = GetBitRange
--
-- toBitVec :: Int -> Expr (IntTy a) -> Expr (BitVec a)
-- toBitVec = ToBitVec
--
-- fromBitVec :: Expr EBitVec -> Expr Int
-- fromBitVec = FromBitVec
--
-- fromInt :: Expr Int -> Expr EReal
-- fromInt = FromInt
--
-- scalar :: EReal -> Expr EReal
-- scalar = Scalar
--
-- (*.) :: Expr EReal -> Expr EVec -> Expr EVec
-- (*.) = ScalarMult
--
-- (.+.) :: Expr b -> Expr b -> Expr b
-- (.+.) = Add
--
-- (.-.) :: Expr b -> Expr b -> Expr b
-- (.-.) = Sub
--
-- (.==.) :: Expr b -> Expr b -> Expr Bool
-- (.==.) = Equal
--
--
