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

import Debug.Trace

import Data.Word
-- import Data.Bits


-- Get the variables in an exp
getVars :: Expr -> [Var]
getVars = Set.toList . go
  where
    go = \case
      SKIP p -> Set.singleton (getPosiVar p)
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
      n = fromIntegral nw
      m = fromIntegral mw
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
rzModer' 0 _ x _ _ = SKIP (Posi x 0)
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
rzSub' x 0 _size _fm = SKIP (Posi x 0)
rzSub' x n size fm =
  let m = n - 1
  in
  rzSub' x m size fm <> if fm ! m then SRR (size - n) x else SKIP (Posi x m)

rzSub :: Var -> Int -> RzValue -> Expr
rzSub x n = rzSub' x n n

rzAdder' :: Var -> Int -> Int -> RzValue -> Expr
rzAdder' x 0 _size _fm = SKIP (Posi x 0)
rzAdder' x n size fm =
  let m = n - 1
  in
  rzAdder' x m size fm <> if fm ! m then SR (size - n) x else SKIP (Posi x m)

rzAdder :: Var -> Int -> RzValue -> Expr
rzAdder x n = Seq (QFT x 0) (Seq (rzAdder' x n n) (RQFT x 0))

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

