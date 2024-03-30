{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module QSym.Interpret
  where

import Prelude hiding (reverse)

import QSym.Syntax
import QSym.QState
import QSym.Utils

import Data.Bits hiding (xor, rotate, rotateR)
import qualified Data.Bits as Bits

interpret :: QEnv Int -> Expr -> QState Value -> QState Value
interpret env expr st =
  case expr of
    SKIP _ -> st
    X p -> update st (fst p) (exchange (atPosi st (fst p)) (snd p))
    CU p e' ->
      if getCUA (atPosi st (fst p)) (snd p)
      then interpret env e' st
      else st
    RZ q p -> update st (fst p) (timesRotate (atPosi st (fst p)) q)
    RRZ q p -> update st (fst p) (timesRotateR (atPosi st p) q)
    SR n x -> update st (fst p) (srRotate (atPosi st x) n)
    SRR n x -> srrRotate st x n
    Lshift x -> update st x (lshift (atPosi st x) x)
    Rshift x -> update st x (rshift (atPosi st x) x)
    Rev x -> update st x (reverse (atPosi st x) x)
    QFT x b -> let n = atVar env x in update st x (turnQFT (atPosi st x) (n-b))
    RQFT x b -> let n = atVar env x in update st x (turnRQFT (atPosi st x) (n-b))
    Seq e1 e2 -> interpret env e2 (interpret env e1 st)

invExpr :: Expr -> Expr
invExpr p =
  case p of
    SKIP a -> SKIP a
    X n -> X n
    CU n p -> CU n (invExpr p)
    SR n x -> SRR n x
    SRR n x -> SR n x
    Lshift x -> Rshift x
    Rshift x -> Lshift x
    Rev x -> Rev x
    RZ q p -> RRZ q p
    RRZ q p -> RZ q p
    QFT x b -> RQFT x b
    RQFT x b -> QFT x b
    Seq p1 p2 -> Seq (invExpr p2) (invExpr p1)

cnot :: Posi -> Posi -> Expr
cnot x y = CU x (X y)

exchange :: Value -> Int -> Value
exchange (NVal b r) p = NVal (complementBit b p) r
exchange v = v

srRotate :: Value -> Int -> Value
srRotate (NVal b r) q = (NVal b r)
srRotate (QVal rc r) q = let n = (atVar env x) in QVal rc (r+(2^(n-1-q)))

srrRotate :: Value -> Int -> Value
srrRotate (NVal b r) q = (NVal b r)
srrRotate (QVal rc r) q = let n = (atVar env x) in QVal rc (r-(2^(n-1-q)))

getCUA :: Value -> Int -> Bool
getCUA (NVal b _) p = testBit b p
getCUA (QVal {}) p = False

timesRotate :: Value -> Int -> Value
timesRotate (NVal b r) p q = let n = (atVar env x) in if testBit b p then NVal b (r+(2^(n-1-q))) else NVal b q
timesRotate (QVal rc r) q = let n = (atVar env x) in QVal rc (r+(2^(n-1-q)))

timesRotateR :: Value -> Int -> Value
timesRotateR (NVal b r) q = let n = (atVar env x) in if testBit b p then NVal b (r-(2^(n-1-q))) else NVal b q
timesRotateR (QVal rc r) q = let n = (atVar env x) in QVal rc (r-(2^(n-1-q)))


-- cutN :: RzValue -> Int -> RzValue
-- cutN r n = r .&. nOnes n
-- RzValue $ \i ->
--   if i < n
--   then r ! i
--   else False

-- fbrev :: Int -> RzValue -> RzValue
-- fbrev n r =
--  mapBitsBelow n r $ \i ->
    r ! (n - 1 - i)
-- RzValue $ \x ->
--   if x < n
--   then r ! (n - 1 - x)
--   else r ! x

-- sumfb :: Bool -> RzValue -> RzValue -> RzValue
-- sumfb b f g = undefined -- TODO: Implement
-- RzValue $ \x ->
--   carry b x f g `xor` (f ! x) `xor` (g ! x)

-- carry :: Bool -> Int -> RzValue -> RzValue -> Bool
-- carry b n f g =
--  case n of
--    0 -> b
--    _ ->
--      let n' = n - 1
--          c = carry b n' f g
--          a = f ! n'
--          b = g ! n'
--      in
--      (a && b) `xor` (b && c) `xor` (a && c)

--xor :: Bool -> Bool -> Bool
--xor = (/=)

lshift :: Value -> Var -> Value
lshift (NVal v r) x = let n = (atVar env x) in NVal (shift v n) r
lshift (QVal v r) x = (QVal v r)

rshift :: Value -> Var -> Value
rshift (NVal v r) x = let n = (atVar env x) in NVal (rotate v n) r
rshift (QVal v r) x = (QVal v r)

reverse :: Value -> Var -> Value
reverse (NVal v r) x = let n = (atVar env x) in NVal (complementBit v n) r
reverse (QVal v r) x = (QVal v r)

turnQFT :: Value -> Value
turnQFT (NVal v r) n = (QVal r (rotate v n))
turnQFT (QVal v r) n = (QVal v f)

turnRQFT :: Value -> Value
turnRQFT (NVal v r) n = (NVal v r)
turnRQFT (QVal rc r) n = (NVal (shift r n) rc)

assignSeq :: QState Value -> Var -> RzValue -> Int -> QState Value
assignSeq st x vals 0 = st
assignSeq st x vals n =
  let m = n - 1
      p = Posi x m
  in
  update (assignSeq st x vals m) p
    (NVal (vals ! m) (getR (atPosi st p)))

-- A function to get the rotation angle of a state.
getR :: Value -> RzValue
getR (NVal _ r) = r
getR (QVal rc _) = rc

assignHR :: QState Value -> Var -> Int -> Int -> QState Value
assignHR = assignH -- TODO: Is this right?

getRQFT :: QState Value -> Var -> RzValue
getRQFT st x =
  case atPosi st (Posi x 0) of
    QVal rc g -> g
    NVal {} -> allFalse

assignR :: QState Value -> Var -> RzValue -> Int -> QState Value
assignR st _x _r 0 = st
assignR st x r n =
  let m = n - 1
      p = Posi x m
  in
  update (assignR st x r m) p
    (upQFT (atPosi st p) (lshiftFun r m))

assignH :: QState Value -> Var -> Int -> Int -> QState Value
assignH f _x _n 0 = f
assignH f x n i =
  let m = n - 1
      p = Posi x (n+m)
  in
  update (assignH f x n m) p
    (upH (atPosi f p))

upH :: Value -> Value
upH (NVal True r) = QVal r (rotate allFalse 1)
upH (NVal False r) = QVal r allFalse
upH (QVal r f) = NVal (f ! 0) r

lshiftFun :: RzValue -> Int -> RzValue
lshiftFun = shiftR
-- lshiftFun f n = f `shiftL` n --RzValue $ \i -> f ! (i+n)

upQFT :: Value -> RzValue -> Value
upQFT (NVal b r) f = QVal r f
upQFT a          _ = a

negatem :: Int -> RzValue -> RzValue
negatem i f =
  mapBitsBelow i f $ \x ->
    not (f ! x)
-- RzValue $ \x ->
--   if x < i
--   then not (f ! x)
--   else f ! x

nat2fb :: Int -> RzValue
nat2fb 0 = allFalse
nat2fb p | p > 0 = pos2fb p
nat2fb p = error $ "nat2fb: got negative argument: " ++ show p

-- | Precondition: Argument must be positive
pos2fb :: Int -> RzValue
pos2fb 1 = fbPush True allFalse
pos2fb n =
  let (b, n') = unconsBit n
  in
  fbPush b (pos2fb n')

fbPush :: Bool -> RzValue -> RzValue
fbPush b v = rzSetBit (v `shiftL` 1) 0 b
-- RzValue $ \case
--   0 -> b
--   n -> f ! (n - 1)

