{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module QSym.Interpret
  where

import Prelude hiding (reverse)

import QSym.Syntax
-- import QSym.QState
import QSym.Utils
import QSym.Monad

import Data.Bits hiding (xor, rotate, rotateR)
import qualified Data.Bits as Bits

-- need to add an other map, one is a function map, mapping from fvar to closure
-- modify env from var to C / Q (int), 
-- there are two types, C type or Q (int) types,

interpret :: Expr -> QSym ()
interpret expr =
  case expr of
    SKIP _ -> pure ()

    X p -> do
      v <- at (posiVar p)
      update (posiVar p) (exchange v (posiInt p))

    CU p e' -> do
      v <- at (posiVar p)
      if getCUA v (posiInt p)
      then interpret e'
      else pure ()

    RZ (ANum q) p0 -> do -- the q term must be evaluated to ANum q in order to make sense
      p <- at (posiVar p0)
      update (posiVar p0) =<< (timesRotate p (posiInt p) q)

    RRZ (ANum q) p0 -> do
      p <- at (posiVar p0)
      update (posiVar p) =<< (timesRotateR p (posiInt p) q)

    SR (ANum n) x -> do
      size <- atVar x
      v <- at x
      update x =<< (srRotate v n size)
      
    SRR (ANum n) x -> do
      size <- atVar x
      v <- at x
      update x =<< (srrRotate v n size)
      
    QFT x (ANum b) -> do
      size <- atVar x
      v <- at x
      update x =<< (turnQFT v (size - b)) 
    
    RQFT x (ANum b) -> do
      size <- atVar x
      v <- at x
      update x =<< (turnQFT v (size - b)) 
    
    Seq e1 e2 -> interpret env e2 (interpret env e1 st)
    
    IFExp (BValue b) e1 e2 -> if b then interpret e1 else interpret e2
    
    App x el -> let vl = map (\ a -> simpAExp a) e1 in -- the el must contain at least one value
                   do
                   Closure x yl e <- findFEnv x
                   interpret (simpExpr (foldl (\ a b -> case b of (bx,bv) -> substAExp a bx bv) e (zip (x:yl) vl)))
    Fix x y z e -> updateFEnv x =<< Closure y z e

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
exchange v _ = v

srRotate :: Value -> Int -> Int -> Value
srRotate (NVal b r) q _ = (NVal b r)
srRotate (QVal rc r) q n = QVal rc (r+(2^(n-1-q)))

srrRotate :: Value -> Int -> Int -> Value
srrRotate (NVal b r) q _ = (NVal b r)
srrRotate (QVal rc r) q n = QVal rc (r-(2^(n-1-q)))

getCUA :: Value -> Int -> Bool
getCUA (NVal b _) p = testBit b p
getCUA (QVal {}) p = False

-- timesRotate :: Value -> Int -> Value
timesRotate :: Value -> Int -> Int -> QSym Value
timesRotate (NVal b r) n q = do
  -- n <- atVar (posiVar p)
  if testBit b n
    then pure $ NVal b (r+(2^(n-1-q)))
    else pure $ NVal b q

timesRotate (QVal rc r) n q= do
  -- n <- atVar (posiVar p)
  pure $ QVal rc (r+(2^(n-1-q)))

timesRotateR :: Value -> Int -> Value
timesRotateR (NVal b r) n p q = if testBit b p then NVal b (r-(2^(n-1-q))) else NVal b q
timesRotateR (QVal rc r) n p q = QVal rc (r-(2^(n-1-q)))


-- cutN :: RzValue -> Int -> RzValue
-- cutN r n = r .&. nOnes n
-- RzValue $ \i ->
--   if i < n
--   then r ! i
--   else False

-- fbrev :: Int -> RzValue -> RzValue
-- fbrev n r =
--  mapBitsBelow n r $ \i ->
    -- r ! (n - 1 - i)
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

lshift :: Value -> Var -> QSym Value
lshift (NVal v r) x = do
  n <- atVar x
  pure $ NVal (shift v n) r

lshift (QVal v r) x = do
  pure $ QVal v r

rshift :: Value -> Var -> QSym Value
rshift (NVal v r) x = do
  n <- atVar x
  pure $ NVal (rotate v n) r

rshift (QVal v r) x = QVal v r

reverse :: Value -> Var -> QSym Value
reverse (NVal v r) x = do
  n <- atVar x
  pure $ NVal (complementBit v n) r
reverse (QVal v r) x = pure (QVal v r)

turnQFT :: Value -> Int -> Value
turnQFT (NVal v r) n = (QVal r (rotate v n))
turnQFT (QVal v r) n = (QVal v r)

turnRQFT :: Value -> Value
turnRQFT (NVal v r) n = (NVal v r)
turnRQFT (QVal rc r) n = (NVal (shift r n) rc)

assignSeq :: Var -> RzValue -> Int -> QSym ()
assignSeq x vals 0 = pure ()
assignSeq x vals n = do
  let m = n - 1
      p = Posi x m

  r <- getR <$> atPosi p
  assignSeq x vals m
  update p (NVal (vals ! m) r)

-- A function to get the rotation angle of a state.
getR :: Value -> RzValue
getR (NVal _ r) = r
getR (QVal rc _) = rc

assignHR :: Var -> Int -> Int -> QSym ()
assignHR = assignH -- TODO: Is this right?

getRQFT :: Var -> QSym RzValue
getRQFT st x =
  case atPosi st (Posi x 0) of
    QVal rc g -> g
    NVal {} -> allFalse

assignR :: Var -> RzValue -> Int -> QSym ()
assignR _x _r 0 = pure ()
assignR x r n = do
  let m = n - 1
      p = Posi x m

  assignR x r m

  y <- atPosi p

  update p (upQFT y (lshiftFun r m))

assignH :: Var -> Int -> Int -> QSym ()
assignH f _x _n 0 = f
assignH f x n i = do
  let m = n - 1
      p = Posi x (n+m)

  y <- atPosi f p
  assignH f x n m

  update p (upH y)

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



-- move the following to test, they are test cases
cnot :: Posi -> Posi -> Expr
cnot p1 p2 = CU p1 (X p2)

ccx :: Posi -> Posi -> Posi -> Expr
ccx p1 p2 p3 = CU p1 (cnot p2 p3)

maj a b c = Seq (cnot c b) (Seq (cnot c a) (ccx a b c))

uma a b c = Seq (ccx a b c) (Seq (cnot c a) (cnot a b))


--Fixpoint MAJseq' n x y c : exp :=
--  match n with
--  | 0 => MAJ c (y,0) (x,0)
--  | S m => MAJseq' m x y c; MAJ (x, m) (y, n) (x, n)
--  end.
--Definition MAJseq n x y c := MAJseq' (n - 1) x y c.

--Fixpoint UMAseq' n x y c : exp :=
--  match n with
--  | 0 => UMA c (y,0) (x,0)
--  | S m => UMA (x, m) (y,n) (x, n); UMAseq' m x y c
--  end.
--Definition UMAseq n x y c := UMAseq' (n - 1) x y c.

--Definition adder01 n x y c: exp := MAJseq n x y c; UMAseq n x y c.

--rzadder :: Var -> Int -> Int -> [Bool] -> 

--Fixpoint rz_adder' (x:var) (n:nat) (size:nat) (M: nat -> bool) :=
--  match n with 
--  | 0 => SKIP (x,0)
--  | S m => rz_adder' x m size M ; if M m then SR (size - n) x else SKIP (x,m)
--  end.

--Definition rz_adder (x:var) (n:nat) (M:nat -> bool) := rz_adder' x n n M.














