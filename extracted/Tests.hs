module Tests
  (app_div_mod_aout)
  where

import Prelude (Int, Bool (..), Enum (..), (+), Num (..), Ord (..), Eq (..), (&&), mod)

import qualified Data.Bits as Bits
import Data.Bits (Bits)

import Common

lsr :: Bits a => a -> Int -> a
lsr = Bits.shiftR

add :: Int -> Int -> Int
add = (+)

mul :: Int -> Int -> Int
mul = ( * )

sub :: Int -> Int -> Int
sub = \ n m -> max 0 (n-m)

add0 :: Int -> Int -> Int
add0 = (+)

mul0 :: Int -> Int -> Int
mul0 = ( * )

eqb :: Int -> Int -> Bool
eqb = (==)

ltb :: Int -> Int -> Bool
ltb n m =
  (<=) (succ n) m

pow :: Int -> Int -> Int
pow n m =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> succ 0)
    (\m0 -> mul0 n (pow n m0))
    m

-- succ :: Int -> Int
-- succ = Pervasives.succ

of_succ_nat :: Int -> Int
of_succ_nat n =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> 1)
    (\x -> succ (of_succ_nat x))
    n

of_nat :: Int -> Int
of_nat = (\ x -> x)

posi_eq :: Posi -> Posi -> Bool
posi_eq = (==)
  -- case r1 of {
  --   x1 y1 -> case r2 of {
  --              x2 y2 -> (&&) (eqb x1 x2) (eqb y1 y2)}}

allfalse :: Int -> Bool
allfalse _ =
  False

fb_push :: Bool -> (Int -> Bool) -> Int -> Bool
fb_push b f x =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> b)
    (\n -> f n)
    x

pos2fb :: Int -> Int -> Bool
pos2fb p =
  (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `lsr` 1) else f2p1 (p `lsr` 1))
    (\p' -> fb_push True (pos2fb p'))
    (\p' -> fb_push False (pos2fb p'))
    (\_ -> fb_push True allfalse)
    p

n2fb :: Int -> Int -> Bool
n2fb n =
  (\ f0 fp n -> if n==0 then f0 () else fp n)
    (\_ -> allfalse)
    (\p -> pos2fb p)
    n

nat2fb :: Int -> Int -> Bool
nat2fb n =
  n2fb (of_nat n)

cut_n :: (Int -> Bool) -> Int -> Int -> Bool
cut_n f n i =
  case ltb i n of {
   True -> f i;
   False -> allfalse i}

-- data Exp =
--    SKIP Posi
--  | X Posi
--  | CU Posi Exp
--  | RZ Int Posi
--  | RRZ Int Posi
--  | SR Int Var
--  | SRR Int Var
--  | Lshift Var
--  | Rshift Var
--  | Rev Var
--  | QFT Var Int
--  | RQFT Var Int
--  | Seq Exp Exp

cNOT :: Posi -> Posi -> Exp
cNOT x y =
  CU x (X y)

sWAP :: Posi -> Posi -> Exp
sWAP x y =
  case posi_eq x y of {
   True -> SKIP x;
   False -> Seq (Seq (cNOT x y) (cNOT y x)) (cNOT x y)}

findnum' :: Int -> Int -> Int -> Int -> Int
findnum' size x y i =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> i)
    (\n ->
    case (<=) y x of {
     True -> i;
     False ->
      findnum' n (mul (succ (succ 0)) x) y
        (add i (succ 0))})
    size

findnum :: Int -> Int -> Int
findnum x n =
  findnum' n x
    (pow (succ (succ 0)) (sub n (succ 0))) 0

div_two_spec :: (Int -> Bool) -> Int -> Bool
div_two_spec f i =
  f (add i (succ 0))

x_var :: Int
x_var =
  0

y_var :: Int
y_var =
  succ 0

appx_adder' :: Var -> Int -> Int -> (Int -> Bool) -> Exp -> Exp
appx_adder' x n b m acc =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> acc)
    (\m0 ->
    appx_adder' x m0 b m (Seq
      (case (&&) ((<=) n b) (m m0) of {
        True -> SR (sub b n) x;
        False -> SKIP (Pair x m0)}) acc))
    n

appx_adder :: Var -> Int -> Int -> (Int -> Bool) -> Exp
appx_adder x n b m =
  appx_adder' x n b m (SKIP (Pair x 0))

appx_sub' :: Var -> Int -> Int -> (Int -> Bool) -> Exp -> Exp
appx_sub' x n b m acc =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> acc)
    (\m0 ->
    appx_sub' x m0 b m (Seq
      (case (&&) ((<=) n b) (m m0) of {
        True -> SRR (sub b n) x;
        False -> SKIP (Pair x m0)}) acc))
    n

appx_sub :: Var -> Int -> Int -> (Int -> Bool) -> Exp
appx_sub x n b m =
  appx_sub' x n b m (SKIP (Pair x 0))

appx_compare_half3 :: Var -> Int -> Int -> Posi -> (Int -> Bool) -> Exp
appx_compare_half3 x n b c m =
  Seq (Seq (appx_sub x n b m) (RQFT x b)) (sWAP (Pair x 0) c)

rshift_by_swap :: Int -> Var -> Exp
rshift_by_swap n x =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> SKIP (Pair x 0))
    (\m -> Seq (rshift_by_swap m x) (sWAP (Pair x m) (Pair x n)))
    n

lshift_by_swap :: Int -> Var -> Exp
lshift_by_swap n x =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> SKIP (Pair x 0))
    (\m -> Seq (sWAP (Pair x m) (Pair x n)) (lshift_by_swap m x))
    n

nLshift_a :: Var -> Int -> Int -> Exp
nLshift_a x n step =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> SKIP (Pair x 0))
    (\m -> Seq (lshift_by_swap n x) (nLshift_a x n m))
    step

appx_moder'_a :: Int -> Int -> Int -> Var -> Var -> (Int -> Bool) -> Exp
appx_moder'_a i n b x ex m =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> SKIP (Pair x 0))
    (\j -> Seq (Seq (Seq (Seq (Seq (appx_compare_half3 x n b (Pair ex j) m)
    (rshift_by_swap n x)) (QFT x (sub b (succ 0)))) (CU (Pair ex j)
    (appx_adder x n (sub b (succ 0)) m))) (X (Pair ex j)))
    (appx_moder'_a j n (sub b (succ 0)) x ex
      (cut_n (div_two_spec m) n)))
    i

appx_div_mod_a :: Int -> Var -> Var -> Int -> Exp
appx_div_mod_a n x ex m =
  let {i = findnum m (sub n (succ 0))} in
  Seq (Seq (Seq (Seq (Seq (Rev x) (QFT x n))
  (appx_moder'_a (succ i) n n x ex
    (nat2fb (mul (pow (succ (succ 0)) i) m)))) (RQFT x
  (sub n (succ i)))) (nLshift_a x n (succ i))) (Rev x)

app_div_mod_aout :: Int -> Int -> Exp
app_div_mod_aout size =
  appx_div_mod_a (succ size) x_var y_var

