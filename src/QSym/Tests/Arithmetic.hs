{-# LANGUAGE LambdaCase #-}

module QSym.Tests.Arithmetic
  where

import QSym.Syntax
import QSym.Monad
import QSym.Utils
import QSym.QState
import QSym.Interpret

import Test.QuickCheck

import qualified Data.Set as Set

import Data.Bits

import Debug.Trace

import Data.Word
-- import Data.Bits
import Numeric.Natural

-- needed for the checkInitV test to determine the bit size of the random number
-- note: since this import is used in both this file and Monad.hs in the exact 
--       same way, it might be a good idea to implement a helper function.
import Math.NumberTheory.Logarithms

maxVecSizeExponent :: Int
maxVecSizeExponent = 20


-- Get the variables in an exp
getVars :: Expr -> [Var]
getVars = Set.toList . go
  where
    go = \case
      SKIP -> mempty
      X p -> Set.singleton (getPosiVar p)
      CU p e -> Set.insert (getPosiVar p) (go e)
      RZ _ p -> Set.singleton (getPosiVar p)
      RRZ _ p -> Set.singleton (getPosiVar p)
      SR _ x -> Set.singleton x
      SRR _ x -> Set.singleton x
      Lshift x -> Set.singleton x
      Rshift x -> Set.singleton x
      Rev x -> Set.singleton x
      QFT x _ -> Set.singleton x
      RQFT x _ -> Set.singleton x
      Seq e1 e2 -> go e1 <> go e2

divModVars :: Natural -> [Var]
divModVars n = getVars (rzDivModOut n 1)

divModEnv :: Natural -> QEnv Natural
divModEnv n =
  mkQEnv $ take 3 (zip (map Var [0..]) (repeat (n + 1))) -- TODO: Is this okay?

interpretDivMod :: Natural -> Natural -> QState Value -> QState Value
interpretDivMod n m state =
  -- trace (pprExpr (rzDivModOut n m)) $
  let env = divModEnv n
  in
  execQSym env state
    (interpret
      (rzDivModOut n m))


rzDivModEnv :: Natural -> QEnv Natural
rzDivModEnv n = mkQEnv $ zip (map Var [0..2]) (repeat (n + 1))

checkrzDivMod :: Property
checkrzDivMod = 
  forAll (choose (1, 2 ^ maxVecSizeExponent)) $ \(ni :: Int) ->
  forAll (choose (1, 2 ^ maxVecSizeExponent)) $ \(mi :: Int) ->
    let
      n = intToNatural (maxVecSizeExponent)
      m = intToNatural mi
      x = intToNatural ni
      ex = intToNatural 0
      env = rzDivModEnv n
      toValue rz = NVal rz rz
      vars = getVars(rzDivMod n (Var x) (Var ex) m)
      initialState = mkState env [(xVar, toValue (fromIntegral x)), (yVar, toValue (fromIntegral ex))]
      expectedState = mkState env [(xVar, toValue (fromIntegral (x `div` m))),(yVar, toValue (fromIntegral (x `mod` m)))]
      expr = rzDivMod n (Var x) (Var ex) m
    in
    stEquiv vars env
      (execQSym env initialState (interpret expr))
      expectedState





-- checkDivMod :: Property
-- checkDivMod =
--   -- forAll (choose (0, 60)) $ \n ->
--   -- forAll (pure 10) $ \n ->
--   -- forAll (pure 60) $ \n ->
--   -- forAll (choose (1, 2^min n 30 - 1)) $ \m ->
--   -- forAll (genBvector n) $ \vx ->
--   forAll (pure 5) $ \(nw :: Word64) ->
--   forAll (pure 5) $ \(mw :: Word64) ->
--   let n, m :: Natural
--       n = fromNatural nw
--       m = fromNatural mw
--       mkRzValue = toRzValue n
--       toValue rz = NVal rz rz -- TODO: Does this make sense? 
--       env = divModEnv n
--   in
--   forAll (pure (mkRzValue (22 :: Word64))) $ \vx ->
--   stEquiv (divModVars n) env
--     (interpretDivMod n m
--       (mkState env [(xVar, toValue vx)]))
--     (mkState
--       env
--       [(xVar, toValue (vx `mod` mkRzValue mw))
--       ,(yVar, toValue (vx `div` mkRzValue mw))
--       ])

rzDivModOut :: Natural -> Natural -> Expr
rzDivModOut size = rzDivMod (size+1) xVar yVar

-- | x = (x % M, x / M) circuit.
rzDivMod :: Natural -> Var -> Var -> Natural -> Expr
rzDivMod 0 _ _ _ = error "divmod is 0"
rzDivMod n x ex m =
  let i = findNum m (n-1)
  in
  Rev x <>
  QFT x 0 <>
  rzModer' (i + 1) n x ex (mkRzValue' ((2^i) * m)) <>
  invExpr ( QFT x 0)

rzModer' :: Natural -> Natural -> Var -> Var -> RzValue -> Expr
rzModer' 0 _ x _ _ = SKIP
rzModer' i n x ex m =
  let j = i - 1
  in
  rzCompareHalf3 x n (Posi ex j) m <> QFT x 0 <>
  CU (Posi ex j) (rzAdder x n m) <>
  X (Posi ex j) <>
  rzModer' j n x ex ((divTwoSpec m))

divTwoSpec :: RzValue -> RzValue
divTwoSpec v = v `div` 2
  --RzValue $ \i -> f ! (i + 1)

-- compare x < m --> the value is stored in c : 1 or 0
rzCompareHalf3 :: Var -> Natural -> Posi -> RzValue -> Expr
rzCompareHalf3 x n c m =
  let p = Posi x (n-1)
  in
  rzSub x n m <>
  RQFT x 0 <>
  cnot p c
  -- let p = Posi x 0
  -- in
  -- rzSub x n m <>
  -- RQFT x n <>
  -- X p <>
  -- cnot p c <>
  -- X p

rzSub' :: Var -> Natural -> Natural -> RzValue -> Expr
rzSub' x 0 _size _fm = SKIP
rzSub' x n size fm =
  let m = n - 1
  in
  rzSub' x m size fm <> if fm ! m then SRR (size - n) x else SKIP

rzSub :: Var -> Natural -> RzValue -> Expr
rzSub x n = rzSub' x n n

modSub :: Natural -> Natural -> Natural -> Natural
modSub size a b = (a - b + 2^size) `mod` (2^size)

-- x stores 10 , m = 20 --> 5, 2^5, (x-m) ---> 2^5 + 10 - 20 = 22 

rzSub_test :: Var -> Natural -> RzValue -> Expr
rzSub_test x n v =
  block
    [ QFT x 0
    , rzSub' x n n v
    , RQFT x 0
    ]

rzSubEnv :: Natural -> QEnv Natural
rzSubEnv n = mkQEnv $ zip (map Var [0..2]) (repeat (n + 1))

checkrzSub :: Property
checkrzSub = 
  forAll (choose (2, 2 ^ 8)) $ \(xi :: Int) ->
  forAll (choose (2, 2 ^ 8)) $ \(ni :: Int) ->
    let
      x = intToNatural xi
      n = intToNatural ni
      toValue rz = NVal rz rz
      expectedSum = (modSub n n x)
      env = rzSubEnv n
      vars = getVars(rzSub (Var x) n (mkRzValue' n))
      initialState = mkState env [(xVar, toValue (fromIntegral x)), (yVar, toValue (fromIntegral n))]
      expectedState = mkState env [(xVar, toValue (fromIntegral expectedSum))]
      expr = rzSub xVar n (mkRzValue' n)
    in
    stEquiv vars env
      (execQSym env initialState (interpret expr))
      expectedState







rzAdder' :: Var -> Natural -> Natural -> RzValue -> Expr
rzAdder' _ n _ _ | n < 0 = error $ "rzAdder': negative n: " ++ show n
rzAdder' x 0 _size _fm = SKIP
rzAdder' x n size fm =
  let m = n - 1
  in
  rzAdder' x m size fm <> if fm ! m then SR (size - n) x else SKIP

rzAdder :: Var -> Natural -> RzValue -> Expr
rzAdder x n = rzAdder' x n n

rzAdder_test :: Var -> Natural -> RzValue -> Expr
rzAdder_test x n v =
  block
    [ QFT x 0
    , rzAdder' x n n v
    , RQFT x 0
    ]

rzadderEnv :: Natural -> QEnv Natural
rzadderEnv n = mkQEnv $ zip (map Var [0..2]) (repeat (n + 1))

checkrzAdder :: Property
checkrzAdder = 
  forAll (choose (0, 2 ^ maxVecSizeExponent)) $ \(xi :: Int) ->
  forAll (choose (0, 2 ^ maxVecSizeExponent)) $ \(ni :: Int) ->
    let
      x = intToNatural xi
      n = intToNatural ni
      toValue rz = NVal rz rz
      expectedSum = x + n
      env = rzadderEnv n
      vars = getVars(rzAdder (Var x) n (mkRzValue' n))
      initialState = mkState env [(xVar, toValue (fromIntegral x)), (yVar, toValue (fromIntegral n))]
      expectedState = mkState env [(xVar, toValue (fromIntegral expectedSum))]
      expr = rzAdder xVar n (mkRzValue' n)
    in
    stEquiv vars env
      (execQSym env initialState (interpret expr))
      expectedState



--Fixpoint rz_full_adder' (x:var) (n:nat) (size:nat) (y:var) :=
---  match n with
--  | 0 => (SKIP (x,0))
--  | S m => ((CU (y,m) (SR (size - n) x)); rz_full_adder' x m size y)
--  end.
-- Definition rz_full_adder (x:var) (n:nat) (y:var) := rz_full_adder' x n n y.

-- we have two arrays, x and y, and they have the same size (size), x,y ---> x,(x+y)%2^size



---multiplication given array x, and a number M, and an extra qubit array ex (ex starts with 0),
---- result is x,ex ---> x, (x*M) % 2^size

---Fixpoint nat_mult' (n:nat) (size:nat) (x:var) (ex:var) (M:nat->bool) :=
--  match n with 
--  | 0 => SKIP (x,0)
--  | S m => one_cu_adder ex size (x,m) M; 
--          nat_mult' m size x ex (cut_n (times_two_spec M) size)
--  end.
--Definition nat_mult (size:nat) (x:var) (re:var) (M:nat -> bool) := 
--   (Rev x; Rev re) ; QFT re size; nat_mult' size size x re M;
--  RQFT re size; inv_exp ( (Rev x; Rev re)).


findNum :: Natural -> Natural -> Natural
findNum x n = findNum' n x (2 ^ (n-1)) 0

-- Example Circuits that are definable by OQASM.
-- find a number that is great-equal than 2^(n-1), assume that the number is less than 2^n
findNum' :: Natural -> Natural -> Natural -> Natural -> Natural
findNum' 0 _x _y i = i
findNum' size x y i =
  let n = size-1
  in
  if y <= x
  then i
  else findNum' n (2 * x) y (i+1)

fromNatural :: (Integral a, Num b) => a -> b
fromNatural x
  | x >= 0    = fromIntegral x
  | otherwise = error "fromIntegral: negative number"


maj :: Posi -> Posi -> Posi -> Expr
maj a b c = cnot c b <> cnot c a <> ccx a b c

uma :: Posi -> Posi -> Posi -> Expr
uma a b c = ccx a b c <> cnot c a <> cnot a b

majSeq' :: Natural -> Var -> Var -> Posi -> Expr
majSeq' 0 x y c = maj c (Posi y 0) (Posi x 0)
majSeq' n x y c = majSeq' (n - 1) x y c <> maj (Posi x (n - 1)) (Posi y n) (Posi x n)

majSeq :: Natural -> Var -> Var -> Posi -> Expr
majSeq n x y c = majSeq' (n - 1) x y c

umaSeq' :: Natural -> Var -> Var -> Posi -> Expr
umaSeq' 0 x y c = uma c (Posi y 0) (Posi x 0)
umaSeq' n x y c = uma (Posi x (n - 1)) (Posi y n) (Posi x n) <> umaSeq' (n - 1) x y c

umaSeq :: Natural -> Var -> Var -> Posi -> Expr
umaSeq n x y c = umaSeq' (n - 1) x y c


-- Define adder 
adder :: Natural -> Var -> Var -> Posi -> Expr
adder n x y c = trace (show y) $  majSeq n x y c <> umaSeq n x y c



adderEnv :: Natural -> QEnv Natural
adderEnv n = mkQEnv $ zip (map Var [0..2]) (repeat (n + 1))

checkAdder :: Property
checkAdder =
  forAll (choose (2,maxVecSizeExponent)) $ \(vci :: Int) ->
  forAll (choose (2, 2^(vci - 1))) $ \(vxi :: Int) ->
  forAll (choose (2, 2^(vci - 1))) $ \(vyi :: Int) ->
  let 
      n = intToNatural vci
      vx = intToNatural vxi
      vy = intToNatural vyi
      toValue rz = NVal rz rz
      env = adderEnv n
      expectedSum = (vx + vy) `mod` 2^n
      vars = getVars (adder n (Var vx) (Var vy) (Posi zVar 0))
      initialState = mkState env [(xVar, toValue (fromIntegral vx)), (yVar, toValue (fromIntegral vy))]
      expectedState = mkState env [(xVar, toValue (fromIntegral expectedSum))]
  in
  stEquiv vars env
    (execQSym env initialState (interpret (adder n xVar yVar (Posi zVar 0))))
    expectedState

-- |initV (see: https://github.com/inQWIRE/VQO/blob/main/OQASM.v#L431C10-L431C17)
-- Initialize qubits given a bitstring b and empty qubits |0>
--  where:
--    var of type `Posi` is the empty qubit array
--    bits of type `(Natural -> Bool)` is a function that returns a boolean for a natural number index into a bitstring
-- 
-- return of type `Expr`: an AST of expressions that produce |b>
initV :: Posi -> (Natural -> Bool) -> Expr
initV var bits -- should bits instead be a byte string?
  | (posiInt var) == 0 = SKIP
  | otherwise = 
    let next = prevPos(var) in
      if bits(posiInt next) then
        Seq (initV next bits) (X next)
      else
        initV next bits

-- |acceptNaturalInstead converts a function that takes an integer and converts
-- it to a function that takes a natural. This is used by the checkInitV test case.
acceptNaturalInstead :: (Integral a, Num t1) => (t1 -> t2) -> a -> t2
acceptNaturalInstead f = (\nat -> (f (fromInteger (toInteger nat))))

-- [test case]: initV
checkInitV :: Property
checkInitV = 
  forAll (choose (0, 2^maxVecSizeExponent)) $ \(randomNumI :: Int) ->
  let
    -- set up a simple test case for now.
    randomNum = intToNatural randomNumI
    -- we need to know how large randomNum is in bytes so that we can
    -- initialize the qubit properly
    numSizeInBytes = intToNatural (integerLog2 (fromIntegral randomNum) + 1)
    -- helper function to convert a number into an NVal
    toValue rz = NVal rz rz
    -- bits
    -- testBit is a builtin Data.Bits function (https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Bits.html#v:testBit)
    -- acceptNaturalInstead works around the fact that testBit takes an Int as it's "indexing" argument
    bits = acceptNaturalInstead $ testBit randomNum
    -- empty qubits of the required size
    var = Posi xVar (numSizeInBytes + 1)
    vars = getVars (initV var bits)
    -- initialize the environment
    env = mkQEnv [(xVar, 0)]
    -- initial state should be a qubit initialized to 0
    initialState = mkState env [(xVar, toValue (RzValue numSizeInBytes 0))]
    -- expected state is our qubit now set to the test int
    -- this will only change the first RzValue, not the second
    expectedState = mkState env [(xVar, NVal (fromIntegral randomNum) (RzValue numSizeInBytes 0))]
  in
    stEquiv 
      vars 
      env 
      (execQSym env initialState (interpret (initV var bits))) 
      expectedState