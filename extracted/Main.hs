module Main where

import qualified Prelude
import Prelude (IO, undefined, ($), print, Show, const)

import Tests
import Common
import Equiv

data Bool =
   True
 | False

andb :: Bool -> Bool -> Bool
andb b1 b2 =
  case b1 of {
   True -> b2;
   False -> False}

xorb :: Bool -> Bool -> Bool
xorb b1 b2 =
  case b1 of {
   True -> case b2 of {
            True -> False;
            False -> True};
   False -> b2}

negb :: Bool -> Bool
negb b =
  case b of {
   True -> False;
   False -> True}

fst :: (Prod a1 a2) -> a1
fst p =
  case p of {
   Pair x _ -> x}

snd :: (Prod a1 a2) -> a2
snd p =
  case p of {
   Pair _ y -> y}

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

sub :: Nat -> Nat -> Nat
sub n m =
  case n of {
   O -> n;
   S k -> case m of {
           O -> n;
           S l -> sub k l}}

data Positive =
   XI Positive
 | XO Positive
 | XH

data N =
   N0
 | Npos Positive

eqb :: Nat -> Nat -> Bool
eqb n m =
  case n of {
   O -> case m of {
         O -> True;
         S _ -> False};
   S n' -> case m of {
            O -> False;
            S m' -> eqb n' m'}}

leb :: Nat -> Nat -> Bool
leb n m =
  case n of {
   O -> True;
   S n' -> case m of {
            O -> False;
            S m' -> leb n' m'}}

ltb :: Nat -> Nat -> Bool
ltb n m =
  leb (S n) m

succ :: Positive -> Positive
succ x =
  case x of {
   XI p -> XO (succ p);
   XO p -> XI p;
   XH -> XO XH}

of_succ_nat :: Nat -> Positive
of_succ_nat n =
  case n of {
   O -> XH;
   S x -> succ (of_succ_nat x)}

of_nat :: Nat -> N
of_nat n =
  case n of {
   O -> N0;
   S n' -> Npos (of_succ_nat n')}

posi_eq :: Posi -> Posi -> Bool
posi_eq r1 r2 =
  case r1 of {
   Pair x1 y1 -> case r2 of {
                  Pair x2 y2 -> andb (eqb x1 x2) (eqb y1 y2)}}

type Rz_val = Nat -> Bool

data Val =
   Nval Bool Rz_val
 | Qval Rz_val Rz_val

eupdate :: (Posi -> a1) -> Posi -> a1 -> Posi -> a1
eupdate f i x j =
  case posi_eq j i of {
   True -> x;
   False -> f j}

allfalse :: Nat -> Bool
allfalse _ =
  False

fb_push :: Bool -> (Nat -> Bool) -> Nat -> Bool
fb_push b f x =
  case x of {
   O -> b;
   S n -> f n}

pos2fb :: Positive -> Nat -> Bool
pos2fb p =
  case p of {
   XI p' -> fb_push True (pos2fb p');
   XO p' -> fb_push False (pos2fb p');
   XH -> fb_push True allfalse}

n2fb :: N -> Nat -> Bool
n2fb n =
  case n of {
   N0 -> allfalse;
   Npos p -> pos2fb p}

nat2fb :: Nat -> Nat -> Bool
nat2fb n =
  n2fb (of_nat n)

carry :: Bool -> Nat -> (Nat -> Bool) -> (Nat -> Bool) -> Bool
carry b n f g =
  case n of {
   O -> b;
   S n' ->
    let {c = carry b n' f g} in
    let {a = f n'} in
    let {b0 = g n'} in xorb (xorb (andb a b0) (andb b0 c)) (andb a c)}

sumfb :: Bool -> (Nat -> Bool) -> (Nat -> Bool) -> Nat -> Bool
sumfb b f g x =
  xorb (xorb (carry b x f g) (f x)) (g x)

negatem :: Nat -> (Nat -> Bool) -> Nat -> Bool
negatem i f x =
  case ltb x i of {
   True -> negb (f x);
   False -> f x}

cut_n :: (Nat -> Bool) -> Nat -> Nat -> Bool
cut_n f n i =
  case ltb i n of {
   True -> f i;
   False -> allfalse i}

fbrev :: Nat -> (Nat -> a1) -> Nat -> a1
fbrev n f x =
  case ltb x n of {
   True -> f (sub (sub n (S O)) x);
   False -> f x}

addto :: (Nat -> Bool) -> Nat -> Nat -> Bool
addto r n i =
  case ltb i n of {
   True ->
    cut_n (fbrev n (sumfb False (cut_n (fbrev n r) n) (nat2fb (S O)))) n i;
   False -> r i}

addto_n :: (Nat -> Bool) -> Nat -> Nat -> Bool
addto_n r n i =
  case ltb i n of {
   True ->
    cut_n
      (fbrev n (sumfb False (cut_n (fbrev n r) n) (negatem n (nat2fb O)))) n
      i;
   False -> r i}

get_cua :: Val -> Bool
get_cua v =
  case v of {
   Nval x _ -> x;
   Qval _ _ -> False}

get_cus :: Nat -> (Posi -> Val) -> Var -> Nat -> Bool
get_cus n f x i =
  case ltb i n of {
   True -> case f (Pair x i) of {
            Nval b _ -> b;
            Qval _ _ -> False};
   False -> allfalse i}

rotate :: Rz_val -> Nat -> Nat -> Bool
rotate =
  addto

times_rotate :: Val -> Nat -> Val
times_rotate v q =
  case v of {
   Nval b r -> case b of {
                True -> Nval b (rotate r q);
                False -> Nval b r};
   Qval rc r -> Qval rc (rotate r q)}

sr_rotate' :: (Posi -> Val) -> Var -> Nat -> Nat -> Posi -> Val
sr_rotate' st x n size =
  case n of {
   O -> st;
   S m ->
    eupdate (sr_rotate' st x m size) (Pair x m)
      (times_rotate (st (Pair x m)) (sub size m))}

sr_rotate :: (Posi -> Val) -> Var -> Nat -> Posi -> Val
sr_rotate st x n =
  sr_rotate' st x (S n) (S n)

r_rotate :: Rz_val -> Nat -> Nat -> Bool
r_rotate =
  addto_n

times_r_rotate :: Val -> Nat -> Val
times_r_rotate v q =
  case v of {
   Nval b r -> case b of {
                True -> Nval b (r_rotate r q);
                False -> Nval b r};
   Qval rc r -> Qval rc (r_rotate r q)}

srr_rotate' :: (Posi -> Val) -> Var -> Nat -> Nat -> Posi -> Val
srr_rotate' st x n size =
  case n of {
   O -> st;
   S m ->
    eupdate (srr_rotate' st x m size) (Pair x m)
      (times_r_rotate (st (Pair x m)) (sub size m))}

srr_rotate :: (Posi -> Val) -> Var -> Nat -> Posi -> Val
srr_rotate st x n =
  srr_rotate' st x (S n) (S n)

exchange :: Val -> Val
exchange v =
  case v of {
   Nval b r -> Nval (negb b) r;
   Qval _ _ -> v}

lshift' :: Nat -> Nat -> (Posi -> Val) -> Var -> Posi -> Val
lshift' n size f x =
  case n of {
   O -> eupdate f (Pair x O) (f (Pair x size));
   S m -> eupdate (lshift' m size f x) (Pair x n) (f (Pair x m))}

lshift :: (Posi -> Val) -> Var -> Nat -> Posi -> Val
lshift f x n =
  lshift' (sub n (S O)) (sub n (S O)) f x

rshift' :: Nat -> Nat -> (Posi -> Val) -> Var -> Posi -> Val
rshift' n size f x =
  case n of {
   O -> eupdate f (Pair x size) (f (Pair x O));
   S m -> eupdate (rshift' m size f x) (Pair x m) (f (Pair x n))}

rshift :: (Posi -> Val) -> Var -> Nat -> Posi -> Val
rshift f x n =
  rshift' (sub n (S O)) (sub n (S O)) f x

reverse :: (Posi -> Val) -> Var -> Nat -> (Prod Var Nat) -> Val
reverse f x n a =
  case andb (eqb (fst a) x) (ltb (snd a) n) of {
   True -> f (Pair x (sub (sub n (S O)) (snd a)));
   False -> f a}

up_qft :: Val -> (Nat -> Bool) -> Val
up_qft v f =
  case v of {
   Nval _ r -> Qval r f;
   Qval _ _ -> v}

lshift_fun :: (Nat -> Bool) -> Nat -> Nat -> Bool
lshift_fun f n i =
  f (add i n)

get_r :: Val -> Rz_val
get_r v =
  case v of {
   Nval _ r -> r;
   Qval rc _ -> rc}

assign_r :: (Posi -> Val) -> Var -> (Nat -> Bool) -> Nat -> Posi -> Val
assign_r f x r n =
  case n of {
   O -> f;
   S m ->
    eupdate (assign_r f x r m) (Pair x m)
      (up_qft (f (Pair x m)) (lshift_fun r m))}

up_h :: Val -> Val
up_h v =
  case v of {
   Nval b r ->
    case b of {
     True -> Qval r (rotate allfalse (S O));
     False -> Qval r allfalse};
   Qval r f -> Nval (f O) r}

assign_h :: (Posi -> Val) -> Var -> Nat -> Nat -> Posi -> Val
assign_h f x n i =
  case i of {
   O -> f;
   S m ->
    eupdate (assign_h f x n m) (Pair x (add n m))
      (up_h (f (Pair x (add n m))))}

turn_qft :: (Posi -> Val) -> Var -> Nat -> Nat -> Posi -> Val
turn_qft st x b rmax =
  assign_h (assign_r st x (get_cus b st x) b) x b (sub rmax b)

assign_seq :: (Posi -> Val) -> Var -> (Nat -> Bool) -> Nat -> Posi -> Val
assign_seq f x vals n =
  case n of {
   O -> f;
   S m ->
    eupdate (assign_seq f x vals m) (Pair x m) (Nval (vals m)
      (get_r (f (Pair x m))))}

assign_h_r :: (Posi -> Val) -> Var -> Nat -> Nat -> Posi -> Val
assign_h_r f x n i =
  case i of {
   O -> f;
   S m ->
    eupdate (assign_h_r f x n m) (Pair x (add n m))
      (up_h (f (Pair x (add n m))))}

get_r_qft :: (Posi -> Val) -> Var -> Nat -> Bool
get_r_qft f x =
  case f (Pair x O) of {
   Nval _ _ -> allfalse;
   Qval _ g -> g}

turn_rqft :: (Posi -> Val) -> Var -> Nat -> Nat -> Posi -> Val
turn_rqft st x b rmax =
  assign_h_r (assign_seq st x (get_r_qft st x) b) x b (sub rmax b)

exp_sem :: (Var -> Nat) -> Exp -> (Posi -> Val) -> Posi -> Val
exp_sem env e st =
  case e of {
   SKIP _ -> st;
   X p -> eupdate st p (exchange (st p));
   CU p e' ->
    case get_cua (st p) of {
     True -> exp_sem env e' st;
     False -> st};
   RZ q p -> eupdate st p (times_rotate (st p) q);
   RRZ q p -> eupdate st p (times_r_rotate (st p) q);
   SR n x -> sr_rotate st x n;
   SRR n x -> srr_rotate st x n;
   Lshift x -> lshift st x (env x);
   Rshift x -> rshift st x (env x);
   Rev x -> reverse st x (env x);
   QFT x b -> turn_qft st x b (env x);
   RQFT x b -> turn_rqft st x b (env x);
   Seq e1 e2 -> exp_sem env e2 (exp_sem env e1 st)}

main :: IO ()
main = do
  let expr1 = app_div_mod_aout 2 3
  print expr1
  print $ exp_sem (const 0) expr1 (const (Nval True (const False))) (Pair 1 7)

  let z = st_equivb undefined undefined
             (exp_sem undefined undefined undefined undefined)
             undefined

  undefined

