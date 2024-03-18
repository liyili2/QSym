{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module QSym.Interpret
  where

import Prelude hiding (reverse)

import QSym.Syntax
import QSym.QState
import QSym.Utils

import Data.Bits hiding (xor, rotate, rotateR)

interpret :: QEnv Int -> Expr -> QState Value -> QState Value
interpret env expr st =
  case expr of
    SKIP _ -> st
    X p -> update st p (exchange (atPosi st p))
    CU p e' ->
      if getCUA (atPosi st p)
      then interpret env e' st
      else st
    RZ q p -> update st p (timesRotate (atPosi st p) q)
    RRZ q p -> update st p (timesRotateR (atPosi st p) q)
    SR n x -> srRotate st x n
    SRR n x -> srrRotate st x n
    Lshift x -> lshift st x (atVar env x)
    Rshift x -> rshift st x (atVar env x)
    Rev x -> reverse st x (atVar env x)
    QFT x b -> turnQFT st x b (atVar env x)
    RQFT x b -> turnRQFT st x b (atVar env x)
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
    Seq p1 p2 -> Seq (invExpr p1) (invExpr p2)

cnot :: Posi -> Posi -> Expr
cnot x y = CU x (X y)

exchange :: Value -> Value
exchange (NVal b r) = NVal (not b) r
exchange v = v

srRotate' :: QState Value -> Var -> Int -> Int -> QState Value
srRotate' st _x 0 _size = st
srRotate' st x  n size =
  let m = n - 1
      p = Posi x m
  in
  update (srRotate' st x m size) p
    (timesRotate (atPosi st p) (size - m))

srRotate :: QState Value -> Var -> Int -> QState Value
srRotate st x n = srRotate' st x (n + 1) (n + 1)

srrRotate' :: QState Value -> Var -> Int -> Int -> QState Value
srrRotate' st _x 0 _size = st
srrRotate' st x  n size =
  let m = n - 1
      p = Posi x m
  in
  update (srrRotate' st x m size) p
    (timesRotateR (atPosi st p) (size - m))

srrRotate :: QState Value -> Var -> Int -> QState Value
srrRotate st x n = srrRotate' st x (n + 1) (n + 1)

getCUA :: Value -> Bool
getCUA (NVal b _) = b
getCUA (QVal {}) = False

timesRotate :: Value -> Int -> Value
timesRotate (NVal True r) q = NVal True (rotate r q)
timesRotate (NVal False r) q = NVal False r
timesRotate (QVal rc r) q = QVal rc (rotate r q)

rotate :: RzValue -> Int -> RzValue
rotate = addTo

rotateR :: RzValue -> Int -> RzValue
rotateR = addToN

timesRotateR :: Value -> Int -> Value
timesRotateR (NVal True r) q = NVal True (rotateR r q)
timesRotateR (NVal False r) q = NVal False r
timesRotateR (QVal rc r) q = QVal rc (rotateR r q)

addTo :: RzValue -> Int -> RzValue
addTo r n = RzValue $ \i ->
  if i < n
  then (cutN (fbrev n (sumfb False (cutN (fbrev n r) n) (nat2fb 1))) n) ! i
  else r ! i

addToN :: RzValue -> Int -> RzValue
addToN r n = RzValue $ \i ->
  if i < n
  then (cutN (fbrev n (sumfb False (cutN (fbrev n r) n) (negatem n (nat2fb 0)))) n) ! i
  else r ! i

cutN :: RzValue -> Int -> RzValue
cutN r n = RzValue $ \i ->
  if i < n
  then r ! i
  else False

fbrev :: Int -> RzValue -> RzValue
fbrev n r = RzValue $ \x ->
  if x < n
  then r ! (n - 1 - x)
  else r ! x

sumfb :: Bool -> RzValue -> RzValue -> RzValue
sumfb b f g = RzValue $ \x ->
  carry b x f g `xor` (f ! x) `xor` (g ! x)

carry :: Bool -> Int -> RzValue -> RzValue -> Bool
carry b n f g =
  case n of
    0 -> b
    _ ->
      let n' = n - 1
          c = carry b n' f g
          a = f ! n'
          b = g ! n'
      in
      (a && b) `xor` (b && c) `xor` (a && c)

xor :: Bool -> Bool -> Bool
xor = (/=)

lshift' :: Int -> Int -> QState Value -> Var -> QState Value
lshift' 0 size st x =
  update st (Posi x 0) (atPosi st (Posi x size))
lshift' n size st x =
  let m = n - 1
  in
  update (lshift' m size st x) (Posi x n) (atPosi st (Posi x m))

lshift :: QState Value -> Var -> Int -> QState Value
lshift st x n = lshift' (n - 1) (n - 1) st x

rshift' :: Int -> Int -> QState Value -> Var -> QState Value
rshift' 0 size st x =
  update st (Posi x size) (atPosi st (Posi x 0))
rshift' n size st x =
  let m = n - 1
  in
  update (rshift' m size st x) (Posi x m) (atPosi st (Posi x n))

rshift :: QState Value -> Var -> Int -> QState Value
rshift st x n = rshift' (n - 1) (n - 1) st x

reverse :: QState Value -> Var -> Int -> QState Value
reverse st x n = QState $ \a@(Posi y m) ->
  if y == x && m < n
  then atPosi st (Posi x ((n-1) - m))
  else atPosi st a

turnQFT :: QState Value -> Var -> Int -> Int -> QState Value
turnQFT st x b rmax =
  assignH (assignR st x (getCUS b st x) b) x b (rmax - b)

turnRQFT :: QState Value -> Var -> Int -> Int -> QState Value
turnRQFT st x b rmax =
  assignHR (assignSeq st x (getRQFT st x) b) x b (rmax - b)

getCUS :: Int -> QState Value -> Var -> RzValue
getCUS n f x = RzValue $ \i ->
  if i < n
  then
    case atPosi f (Posi x i) of
      NVal b r -> b
      _ -> False
  else False

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
    _ -> allFalse

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
lshiftFun f n = RzValue $ \i -> f ! (i+n)

upQFT :: Value -> RzValue -> Value
upQFT (NVal b r) f = QVal r f
upQFT a          _ = a

negatem :: Int -> RzValue -> RzValue
negatem i f = RzValue $ \x ->
  if x < i
  then not (f ! x)
  else f ! x

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
fbPush b f = RzValue $ \case
  0 -> b
  n -> f ! (n - 1)

