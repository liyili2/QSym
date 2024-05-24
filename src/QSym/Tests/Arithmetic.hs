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

divModVars :: Int -> [Var]
divModVars n = getVars (rzDivModOut n 1)

divModEnv :: Int -> QEnv Int
divModEnv n =
  mkQEnv $ take 3 (zip (map Var [0..]) (repeat (n + 1))) -- TODO: Is this okay?

interpretDivMod :: Int -> Int -> QState Value -> QState Value
interpretDivMod n m state =
  -- trace (pprExpr (rzDivModOut n m)) $
  let env = divModEnv n
  in
  execQSym env state
    (interpret
      (rzDivModOut n m))

checkDivMod :: Property
checkDivMod =
  -- forAll (choose (0, 60)) $ \n ->
  -- forAll (pure 10) $ \n ->
  -- forAll (pure 60) $ \n ->
  -- forAll (choose (1, 2^min n 30 - 1)) $ \m ->
  -- forAll (genBvector n) $ \vx ->
  forAll (pure 5) $ \(nw :: Word64) ->
  forAll (pure 5) $ \(mw :: Word64) ->
  let n, m :: Int
      n = fromNatural nw
      m = fromNatural mw
      mkRzValue = toRzValue n
      toValue rz = NVal rz rz -- TODO: Does this make sense? 
      env = divModEnv n
  in
  forAll (pure (mkRzValue (22 :: Word64))) $ \vx ->
  stEquiv (divModVars n) env
    (interpretDivMod n m
      (mkState env [(xVar, toValue vx)]))
    (mkState
      env
      [(xVar, toValue (vx `mod` mkRzValue mw))
      ,(yVar, toValue (vx `div` mkRzValue mw))
      ])

rzDivModOut :: Int -> Int -> Expr
rzDivModOut size = rzDivMod (size+1) xVar yVar

-- | x = (x % M, x / M) circuit.
rzDivMod :: Int -> Var -> Var -> Int -> Expr
rzDivMod n x ex m =
  let i = findNum m (n-1)
  in
  Rev x <>
  QFT x n <>
  rzModer' (i + 1) n x ex (mkRzValue' ((2^i) * m)) <>
  invExpr (Rev x <> QFT x n)

rzModer' :: Int -> Int -> Var -> Var -> RzValue -> Expr
rzModer' 0 _ x _ _ = SKIP
rzModer' i n x ex m =
  let j = i - 1
  in
  rzCompareHalf3 x n (Posi ex j) m <> QFT x n <>
  CU (Posi ex j) (rzAdder x n m) <>
  X (Posi ex j) <>
  rzModer' j n x ex (cutN (divTwoSpec m) n)

divTwoSpec :: RzValue -> RzValue
divTwoSpec v = v `div` 2
  --RzValue $ \i -> f ! (i + 1)

-- compare x < m
rzCompareHalf3 :: Var -> Int -> Posi -> RzValue -> Expr
rzCompareHalf3 x n c m =
  let p = Posi x 0
  in
  rzSub x n m <>
  RQFT x n <>
  cnot p c
  -- let p = Posi x 0
  -- in
  -- rzSub x n m <>
  -- RQFT x n <>
  -- X p <>
  -- cnot p c <>
  -- X p

rzSub' :: Var -> Int -> Int -> RzValue -> Expr
rzSub' x 0 _size _fm = SKIP
rzSub' x n size fm =
  let m = n - 1
  in
  rzSub' x m size fm <> if fm ! m then SRR (size - n) x else SKIP

rzSub :: Var -> Int -> RzValue -> Expr
rzSub x n = rzSub' x n n

rzAdder' :: Var -> Int -> Int -> RzValue -> Expr
rzAdder' x 0 _size _fm = SKIP
rzAdder' x n size fm =
  let m = n - 1
  in
  rzAdder' x m size fm <> if fm ! m then SR (size - n) x else SKIP

rzAdder :: Var -> Int -> RzValue -> Expr
rzAdder x n = rzAdder' x n n

rzAdder_test :: Var -> Int -> RzValue -> Expr
rzAdder_test x n v =
  block
    [ QFT x 0
    , rzAdder' x n n v
    , RQFT x 0
    ]

findNum :: Int -> Int -> Int
findNum x n = findNum' n x (2 ^ (n-1)) 0

-- Example Circuits that are definable by OQASM.
-- find a number that is great-equal than 2^(n-1), assume that the number is less than 2^n
findNum' :: Int -> Int -> Int -> Int -> Int
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


-- Define MAJ and UMA
maj :: Posi -> Posi -> Posi -> Expr
maj a b c = cnot c b <> cnot c a <> ccx a b c

uma :: Posi -> Posi -> Posi -> Expr
uma a b c = ccx a b c <> cnot c a <> cnot a b

-- Define MAJseq and UMAseq
majSeq' :: Int -> Var -> Var -> Posi -> Expr
majSeq' 0 x y c = maj c (Posi y 0) (Posi x 0)
majSeq' n x y c = majSeq' (n - 1) x y c <> maj (Posi x (n - 1)) (Posi y n) (Posi x n)

majSeq :: Int -> Var -> Var -> Posi -> Expr
majSeq n x y c = majSeq' (n - 1) x y c

umaSeq' :: Int -> Var -> Var -> Posi -> Expr
umaSeq' 0 x y c = uma c (Posi y 0) (Posi x 0)
umaSeq' n x y c = uma (Posi x (n - 1)) (Posi y n) (Posi x n) <> umaSeq' (n - 1) x y c

umaSeq :: Int -> Var -> Var -> Posi -> Expr
umaSeq n x y c = umaSeq' (n - 1) x y c


-- Define adder 
adder :: Int -> Var -> Var -> Posi -> Expr
adder n x y c = trace ("Calling fib " ++ show y) $  majSeq n x y c <> umaSeq n x y c



-- Define the environment for the adder test
adderEnv :: Int -> QEnv Int
adderEnv n = mkQEnv $ zip (map Var [0..2]) (repeat (n + 1))

-- Test property for the adder function
checkAdder :: Property
checkAdder = 
  forAll (choose (1, 60)) $ \(n :: Int) ->
  forAll (choose (0, 2^(n - 1))) $ \(vx :: Int) ->
  forAll (choose (0, 2^(n - 1))) $ \(vy :: Int) ->
  let 
      n = n
      mkRzValue = toRzValue n
      toValue rz = NVal rz rz
      env = adderEnv n
      expectedSum = vx + vy
      vars = getVars (adder n xVarv yVarv (Posi cVarv 0))
      initialState = mkState env [(xVarv, toValue (fromIntegral vx)), (yVarv, toValue (fromIntegral vy))]
      expectedState = mkState env [(xVarv, toValue (fromIntegral expectedSum))]
  in
  stEquiv vars env
    (execQSym env initialState (interpret (adder n xVarv yVarv (Posi cVarv 0))))
    expectedState
-- Variables for the adder test
xVarv :: Var
xVarv = Var 0

yVarv :: Var
yVarv = Var 1

cVarv :: Var
cVarv = Var 2

-- mkState :: QEnv Int -> [(Var, Value)] -> QState Value
-- mkState env vars = QState { qEnv = env, qVars = M.fromList vars }

-- stEquiv :: [Var] -> QEnv Int -> QState Value -> QState Value -> Bool
-- stEquiv vars env st1 st2 = all (\v -> stateGet v st1 == stateGet v st2) vars