module Tests
  -- (app_div_mod_aout)
  (rz_div_mod_out
  ,div_mod_env
  ,div_mod_vars
  ,Tree (..))
  where

import qualified Prelude
import Prelude (Int, Bool (..), Enum (..), (+), Num (..), Ord ((<), (>), (<=), (>=)), Eq (..), (&&), mod, Maybe (..), min, max)

import qualified Data.Bits as Bits
import Data.Bits (Bits, shiftR)

import Common

(~-) :: a -> a
(~-) = Prelude.id

fst :: (( , ) a1 a2) -> a1
fst p =
  case p of {
    (x, _) -> x}

snd :: (( , ) a1 a2) -> a2
snd p =
  case p of {
    (_, y) -> y}

app :: ([] a1) -> ([] a1) -> [] a1
app l m =
  case l of {
   [] -> m;
   ( : ) a l1 -> ( : ) a (app l1 m)}

data Comparison =
   Eq
 | Lt
 | Gt

data CompareSpecT =
   CompEqT
 | CompLtT
 | CompGtT

compareSpec2Type :: Comparison -> CompareSpecT
compareSpec2Type c =
  case c of {
   Eq -> CompEqT;
   Lt -> CompLtT;
   Gt -> CompGtT}

type CompSpecT a = CompareSpecT

compSpec2Type :: a1 -> a1 -> Comparison -> CompSpecT a1
compSpec2Type _ _ =
  compareSpec2Type

add :: Int -> Int -> Int
add = (+)

mul :: Int -> Int -> Int
mul = (*)

sub :: Int -> Int -> Int
sub = \ n m -> max 0 (n-m)

add0 :: Int -> Int -> Int
add0 = (+)

mul0 :: Int -> Int -> Int
mul0 = (*)

ltb :: Int -> Int -> Bool
ltb n m =
  (<=) (succ n) m

compare :: Int -> Int -> Comparison
compare = \ n m -> if n==m then Eq else if n<m then Lt else Gt

pow :: Int -> Int -> Int
pow n m =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> succ 0)
    (\m0 -> mul0 n (pow n m0))
    m

-- succ :: Int -> Int
-- succ = Pervasives.succ

add1 :: Int -> Int -> Int
add1 = (+)

add_carry :: Int -> Int -> Int
add_carry x y =
  (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
    (\p ->
    (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
      (\q -> (\ p->1+2*p) (add_carry p q))
      (\q -> (\ p->2*p) (add_carry p q))
      (\_ -> (\ p->1+2*p) (succ p))
      y)
    (\p ->
    (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
      (\q -> (\ p->2*p) (add_carry p q))
      (\q -> (\ p->1+2*p) (add1 p q))
      (\_ -> (\ p->2*p) (succ p))
      y)
    (\_ ->
    (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
      (\q -> (\ p->1+2*p) (succ q))
      (\q -> (\ p->2*p) (succ q))
      (\_ -> (\ p->1+2*p) 1)
      y)
    x

pred_double :: Int -> Int
pred_double x =
  (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
    (\p -> (\ p->1+2*p) ((\ p->2*p) p))
    (\p -> (\ p->1+2*p) (pred_double p))
    (\_ -> 1)
    x

compare_cont :: Comparison -> Int -> Int -> Comparison
compare_cont = \ c x y -> if x==y then c else if x<y then Lt else Gt

compare0 :: Int -> Int -> Comparison
compare0 = \ x y -> if x==y then Eq else if x<y then Lt else Gt

of_succ_nat :: Int -> Int
of_succ_nat n =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> 1)
    (\x -> succ (of_succ_nat x))
    n

of_nat :: Int -> Int
of_nat = (\ x -> x)

double :: Int -> Int
double x =
  (\ f0 fp fn z -> if z==0 then f0 () else if z>0 then fp z else fn (-z))
    (\_ -> 0)
    (\p ->  ((\ p->2*p) p))
    (\p -> (~-) ((\ p->2*p) p))
    x

succ_double :: Int -> Int
succ_double x =
  (\ f0 fp fn z -> if z==0 then f0 () else if z>0 then fp z else fn (-z))
    (\_ ->  1)
    (\p ->  ((\ p->1+2*p) p))
    (\p -> (~-) (pred_double p))
    x

pred_double0 :: Int -> Int
pred_double0 x =
  (\ f0 fp fn z -> if z==0 then f0 () else if z>0 then fp z else fn (-z))
    (\_ -> (~-) 1)
    (\p ->  (pred_double p))
    (\p -> (~-) ((\ p->1+2*p) p))
    x

pos_sub :: Int -> Int -> Int
pos_sub x y =
  (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
    (\p ->
    (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
      (\q -> double (pos_sub p q))
      (\q -> succ_double (pos_sub p q))
      (\_ ->  ((\ p->2*p) p))
      y)
    (\p ->
    (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
      (\q -> pred_double0 (pos_sub p q))
      (\q -> double (pos_sub p q))
      (\_ ->  (pred_double p))
      y)
    (\_ ->
    (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
      (\q -> (~-) ((\ p->2*p) q))
      (\q -> (~-) (pred_double q))
      (\_ -> 0)
      y)
    x

add2 :: Int -> Int -> Int
add2 = (+)

compare1 :: Int -> Int -> Comparison
compare1 = \ x y -> if x==y then Eq else if x<y then Lt else Gt

leb :: Int -> Int -> Bool
leb x y =
  case compare1 x y of {
   Gt -> False;
   _ -> True}

ltb0 :: Int -> Int -> Bool
ltb0 x y =
  case compare1 x y of {
   Lt -> True;
   _ -> False}

-- max :: Int -> Int -> Int
-- max = Pervasives.max

data Compare x =
   LT
 | EQ
 | GT

type T = Int

_0 :: Int
_0 =
  0

_1 :: Int
_1 =
   1

_2 :: Int
_2 =
   ((\ p->2*p) 1)

add3 :: Int -> Int -> Int
add3 =
  add2

max0 :: Int -> Int -> Int
max0 =
  max

ltb1 :: Int -> Int -> Bool
ltb1 =
  ltb0

leb0 :: Int -> Int -> Bool
leb0 =
  leb

-- type Var = Int

-- type Posi = ( , ) Var Int

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
  (\ f2p1 f2p f1 p -> if p==1 then f1 () else if p `mod` 2 == 0 then f2p (p `shiftR` 1) else f2p1 (p `shiftR` 1))
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

compare2 :: Int -> Int -> Compare Int
compare2 x y =
  case compare x y of {
   Eq -> EQ;
   Lt -> LT;
   Gt -> GT}

eq_dec :: Int -> Int -> Bool
eq_dec =
  (==)

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

inv_exp :: Exp -> Exp
inv_exp p =
  case p of {
   CU n p0 -> CU n (inv_exp p0);
   RZ q p1 -> RRZ q p1;
   RRZ q p1 -> RZ q p1;
   SR n x -> SRR n x;
   SRR n x -> SR n x;
   Lshift x -> Rshift x;
   Rshift x -> Lshift x;
   QFT x b -> RQFT x b;
   RQFT x b -> QFT x b;
   Seq p1 p2 -> Seq (inv_exp p2) (inv_exp p1);
   x -> x}

cNOT :: Posi -> Posi -> Exp
cNOT x y =
  CU x (X y)

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

type T0 = Int

eq_dec0 :: Int -> Int -> Bool
eq_dec0 =
  eq_dec

compare3 :: Int -> Int -> Comparison
compare3 x y =
  case compare2 x y of {
   LT -> Lt;
   EQ -> Eq;
   GT -> Gt}

type Elt = Int

data Tree =
   Leaf
 | Node T Tree Int Tree

empty :: Tree
empty =
  Leaf

is_empty :: Tree -> Bool
is_empty t =
  case t of {
   Leaf -> True;
   Node _ _ _ _ -> False}

mem :: Int -> Tree -> Bool
mem x t =
  case t of {
   Leaf -> False;
   Node _ l k r ->
    case compare2 x k of {
     LT -> mem x l;
     EQ -> True;
     GT -> mem x r}}

min_elt :: Tree -> Maybe Elt
min_elt t =
  case t of {
   Leaf -> Nothing;
   Node _ l x _ -> case l of {
                    Leaf -> Just x;
                    Node _ _ _ _ -> min_elt l}}

max_elt :: Tree -> Maybe Elt
max_elt t =
  case t of {
   Leaf -> Nothing;
   Node _ _ x r -> case r of {
                    Leaf -> Just x;
                    Node _ _ _ _ -> max_elt r}}

choose :: Tree -> Maybe Elt
choose =
  min_elt

fold :: (Elt -> a1 -> a1) -> Tree -> a1 -> a1
fold f t base =
  case t of {
   Leaf -> base;
   Node _ l x r -> fold f r (f x (fold f l base))}

elements_aux :: ([] Int) -> Tree -> [] Int
elements_aux acc s =
  case s of {
   Leaf -> acc;
   Node _ l x r -> elements_aux (( : ) x (elements_aux acc r)) l}

elements :: Tree -> [] Int
elements =
  elements_aux []

rev_elements_aux :: ([] Int) -> Tree -> [] Int
rev_elements_aux acc s =
  case s of {
   Leaf -> acc;
   Node _ l x r -> rev_elements_aux (( : ) x (rev_elements_aux acc l)) r}

rev_elements :: Tree -> [] Int
rev_elements =
  rev_elements_aux []

cardinal :: Tree -> Int
cardinal s =
  case s of {
   Leaf -> 0;
   Node _ l _ r -> succ (add (cardinal l) (cardinal r))}

maxdepth :: Tree -> Int
maxdepth s =
  case s of {
   Leaf -> 0;
   Node _ l _ r -> succ (max (maxdepth l) (maxdepth r))}

mindepth :: Tree -> Int
mindepth s =
  case s of {
   Leaf -> 0;
   Node _ l _ r -> succ (min (mindepth l) (mindepth r))}

for_all :: (Elt -> Bool) -> Tree -> Bool
for_all f s =
  case s of {
   Leaf -> True;
   Node _ l x r ->
    case case f x of {
          True -> for_all f l;
          False -> False} of {
     True -> for_all f r;
     False -> False}}

exists_ :: (Elt -> Bool) -> Tree -> Bool
exists_ f s =
  case s of {
   Leaf -> False;
   Node _ l x r ->
    case case f x of {
          True -> True;
          False -> exists_ f l} of {
     True -> True;
     False -> exists_ f r}}

data Enumeration =
   End
 | More Elt Tree Enumeration

cons :: Tree -> Enumeration -> Enumeration
cons s e =
  case s of {
   Leaf -> e;
   Node _ l x r -> cons l (More x r e)}

compare_more :: Int -> (Enumeration -> Comparison) -> Enumeration ->
                Comparison
compare_more x1 cont e2 =
  case e2 of {
   End -> Gt;
   More x2 r2 e3 ->
    case compare2 x1 x2 of {
     LT -> Lt;
     EQ -> cont (cons r2 e3);
     GT -> Gt}}

compare_cont0 :: Tree -> (Enumeration -> Comparison) -> Enumeration ->
                 Comparison
compare_cont0 s1 cont e2 =
  case s1 of {
   Leaf -> cont e2;
   Node _ l1 x1 r1 ->
    compare_cont0 l1 (compare_more x1 (compare_cont0 r1 cont)) e2}

compare_end :: Enumeration -> Comparison
compare_end e2 =
  case e2 of {
   End -> Eq;
   More _ _ _ -> Lt}

compare4 :: Tree -> Tree -> Comparison
compare4 s1 s2 =
  compare_cont0 s1 compare_end (cons s2 End)

equal :: Tree -> Tree -> Bool
equal s1 s2 =
  case compare4 s1 s2 of {
   Eq -> True;
   _ -> False}

subsetl :: (Tree -> Bool) -> Int -> Tree -> Bool
subsetl subset_l1 x1 s2 =
  case s2 of {
   Leaf -> False;
   Node _ l2 x2 r2 ->
    case compare2 x1 x2 of {
     LT -> subsetl subset_l1 x1 l2;
     EQ -> subset_l1 l2;
     GT -> case mem x1 r2 of {
            True -> subset_l1 s2;
            False -> False}}}

subsetr :: (Tree -> Bool) -> Int -> Tree -> Bool
subsetr subset_r1 x1 s2 =
  case s2 of {
   Leaf -> False;
   Node _ l2 x2 r2 ->
    case compare2 x1 x2 of {
     LT -> case mem x1 l2 of {
            True -> subset_r1 s2;
            False -> False};
     EQ -> subset_r1 r2;
     GT -> subsetr subset_r1 x1 r2}}

subset :: Tree -> Tree -> Bool
subset s1 s2 =
  case s1 of {
   Leaf -> True;
   Node _ l1 x1 r1 ->
    case s2 of {
     Leaf -> False;
     Node _ l2 x2 r2 ->
      case compare2 x1 x2 of {
       LT ->
        case subsetl (subset l1) x1 l2 of {
         True -> subset r1 s2;
         False -> False};
       EQ -> case subset l1 l2 of {
              True -> subset r1 r2;
              False -> False};
       GT ->
        case subsetr (subset r1) x1 r2 of {
         True -> subset l1 s2;
         False -> False}}}}

type T1 = Tree

height :: T1 -> T
height s =
  case s of {
   Leaf -> _0;
   Node h _ _ _ -> h}

singleton :: Int -> Tree
singleton x =
  Node _1 Leaf x Leaf

create :: T1 -> Int -> T1 -> Tree
create l x r =
  Node (add3 (max0 (height l) (height r)) _1) l x r

assert_false :: T1 -> Int -> T1 -> Tree
assert_false =
  create

bal :: T1 -> Int -> T1 -> Tree
bal l x r =
  let {hl = height l} in
  let {hr = height r} in
  case ltb1 (add3 hr _2) hl of {
   True ->
    case l of {
     Leaf -> assert_false l x r;
     Node _ ll lx lr ->
      case leb0 (height lr) (height ll) of {
       True -> create ll lx (create lr x r);
       False ->
        case lr of {
         Leaf -> assert_false l x r;
         Node _ lrl lrx lrr -> create (create ll lx lrl) lrx (create lrr x r)}}};
   False ->
    case ltb1 (add3 hl _2) hr of {
     True ->
      case r of {
       Leaf -> assert_false l x r;
       Node _ rl rx rr ->
        case leb0 (height rl) (height rr) of {
         True -> create (create l x rl) rx rr;
         False ->
          case rl of {
           Leaf -> assert_false l x r;
           Node _ rll rlx rlr ->
            create (create l x rll) rlx (create rlr rx rr)}}};
     False -> create l x r}}

add4 :: Int -> Tree -> Tree
add4 x s =
  case s of {
   Leaf -> Node _1 Leaf x Leaf;
   Node h l y r ->
    case compare2 x y of {
     LT -> bal (add4 x l) y r;
     EQ -> Node h l y r;
     GT -> bal l y (add4 x r)}}

join :: Tree -> Elt -> T1 -> T1
join l =
  case l of {
   Leaf -> add4;
   Node lh ll lx lr -> (\x ->
    let {
     join_aux r =
       case r of {
        Leaf -> add4 x l;
        Node rh rl rx rr ->
         case ltb1 (add3 rh _2) lh of {
          True -> bal ll lx (join lr x r);
          False ->
           case ltb1 (add3 lh _2) rh of {
            True -> bal (join_aux rl) rx rr;
            False -> create l x r}}}}
    in join_aux)}

remove_min :: Tree -> Elt -> T1 -> ( , ) T1 Elt
remove_min l x r =
  case l of {
   Leaf -> (,) r x;
   Node _ ll lx lr -> case remove_min ll lx lr of {
                        (l', m) -> (,) (bal l' x r) m}}

merge :: Tree -> Tree -> Tree
merge s1 s2 =
  case s1 of {
   Leaf -> s2;
   Node _ _ _ _ ->
    case s2 of {
     Leaf -> s1;
     Node _ l2 x2 r2 -> case remove_min l2 x2 r2 of {
                          (s2', m) -> bal s1 m s2'}}}

remove :: Int -> Tree -> Tree
remove x s =
  case s of {
   Leaf -> Leaf;
   Node _ l y r ->
    case compare2 x y of {
     LT -> bal (remove x l) y r;
     EQ -> merge l r;
     GT -> bal l y (remove x r)}}

concat :: Tree -> Tree -> Tree
concat s1 s2 =
  case s1 of {
   Leaf -> s2;
   Node _ _ _ _ ->
    case s2 of {
     Leaf -> s1;
     Node _ l2 x2 r2 ->
      case remove_min l2 x2 r2 of {
        (s2', m) -> join s1 m s2'}}}

data Triple =
   Mktriple T1 Bool T1

t_left :: Triple -> T1
t_left t =
  case t of {
   Mktriple t_left0 _ _ -> t_left0}

t_in :: Triple -> Bool
t_in t =
  case t of {
   Mktriple _ t_in0 _ -> t_in0}

t_right :: Triple -> T1
t_right t =
  case t of {
   Mktriple _ _ t_right0 -> t_right0}

split :: Int -> Tree -> Triple
split x s =
  case s of {
   Leaf -> Mktriple Leaf False Leaf;
   Node _ l y r ->
    case compare2 x y of {
     LT ->
      case split x l of {
       Mktriple ll b rl -> Mktriple ll b (join rl y r)};
     EQ -> Mktriple l True r;
     GT ->
      case split x r of {
       Mktriple rl b rr -> Mktriple (join l y rl) b rr}}}

inter :: Tree -> Tree -> Tree
inter s1 s2 =
  case s1 of {
   Leaf -> Leaf;
   Node _ l1 x1 r1 ->
    case s2 of {
     Leaf -> Leaf;
     Node _ _ _ _ ->
      case split x1 s2 of {
       Mktriple l2' pres r2' ->
        case pres of {
         True -> join (inter l1 l2') x1 (inter r1 r2');
         False -> concat (inter l1 l2') (inter r1 r2')}}}}

diff :: Tree -> Tree -> Tree
diff s1 s2 =
  case s1 of {
   Leaf -> Leaf;
   Node _ l1 x1 r1 ->
    case s2 of {
     Leaf -> s1;
     Node _ _ _ _ ->
      case split x1 s2 of {
       Mktriple l2' pres r2' ->
        case pres of {
         True -> concat (diff l1 l2') (diff r1 r2');
         False -> join (diff l1 l2') x1 (diff r1 r2')}}}}

union :: Tree -> Tree -> Tree
union s1 s2 =
  case s1 of {
   Leaf -> s2;
   Node _ l1 x1 r1 ->
    case s2 of {
     Leaf -> s1;
     Node _ _ _ _ ->
      case split x1 s2 of {
       Mktriple l2' _ r2' -> join (union l1 l2') x1 (union r1 r2')}}}

filter :: (Elt -> Bool) -> Tree -> Tree
filter f s =
  case s of {
   Leaf -> Leaf;
   Node _ l x r ->
    let {l' = filter f l} in
    let {r' = filter f r} in
    case f x of {
     True -> join l' x r';
     False -> concat l' r'}}

partition :: (Elt -> Bool) -> T1 -> ( , ) T1 T1
partition f s =
  case s of {
   Leaf -> (,) Leaf Leaf;
   Node _ l x r ->
    case partition f l of {
      (l1, l2) ->
      case partition f r of {
        (r1, r2) ->
        case f x of {
         True -> (,) (join l1 x r1) (concat l2 r2);
         False -> (,) (concat l1 r1) (join l2 x r2)}}}}

ltb_tree :: Int -> Tree -> Bool
ltb_tree x s =
  case s of {
   Leaf -> True;
   Node _ l y r ->
    case compare2 x y of {
     GT -> (&&) (ltb_tree x l) (ltb_tree x r);
     _ -> False}}

gtb_tree :: Int -> Tree -> Bool
gtb_tree x s =
  case s of {
   Leaf -> True;
   Node _ l y r ->
    case compare2 x y of {
     LT -> (&&) (gtb_tree x l) (gtb_tree x r);
     _ -> False}}

isok :: Tree -> Bool
isok s =
  case s of {
   Leaf -> True;
   Node _ l x r ->
    (&&) ((&&) ((&&) (isok l) (isok r)) (ltb_tree x l)) (gtb_tree x r)}

type T2 = Int

compare5 :: Int -> Int -> Comparison
compare5 x y =
  case compare2 x y of {
   LT -> Lt;
   EQ -> Eq;
   GT -> Gt}

eq_dec1 :: Int -> Int -> Bool
eq_dec1 =
  eq_dec0

type T3 = Int

compare6 :: Int -> Int -> Comparison
compare6 x y =
  case compare2 x y of {
   LT -> Lt;
   EQ -> Eq;
   GT -> Gt}

eq_dec2 :: Int -> Int -> Bool
eq_dec2 =
  eq_dec1

eq_dec3 :: Int -> Int -> Bool
eq_dec3 =
  eq_dec0

lt_dec :: Int -> Int -> Bool
lt_dec x y =
  let {
   c = compSpec2Type x y
         (case compare2 x y of {
           LT -> Lt;
           EQ -> Eq;
           GT -> Gt})}
  in
  case c of {
   CompLtT -> True;
   _ -> False}

eqb :: Int -> Int -> Bool
eqb x y =
  case eq_dec3 x y of {
   True -> True;
   False -> False}

data R_min_elt =
   R_min_elt_0 Tree
 | R_min_elt_1 Tree T Tree Int Tree
 | R_min_elt_2 Tree T Tree Int Tree T Tree Int Tree (Maybe Elt) R_min_elt

data R_max_elt =
   R_max_elt_0 Tree
 | R_max_elt_1 Tree T Tree Int Tree
 | R_max_elt_2 Tree T Tree Int Tree T Tree Int Tree (Maybe Elt) R_max_elt

type T4 = Int

compare7 :: Int -> Int -> Comparison
compare7 x y =
  case compare2 x y of {
   LT -> Lt;
   EQ -> Eq;
   GT -> Gt}

eq_dec4 :: Int -> Int -> Bool
eq_dec4 =
  eq_dec0

type T5 = Int

compare8 :: Int -> Int -> Comparison
compare8 x y =
  case compare2 x y of {
   LT -> Lt;
   EQ -> Eq;
   GT -> Gt}

eq_dec5 :: Int -> Int -> Bool
eq_dec5 =
  eq_dec4

eq_dec6 :: Int -> Int -> Bool
eq_dec6 =
  eq_dec0

lt_dec0 :: Int -> Int -> Bool
lt_dec0 x y =
  let {
   c = compSpec2Type x y
         (case compare2 x y of {
           LT -> Lt;
           EQ -> Eq;
           GT -> Gt})}
  in
  case c of {
   CompLtT -> True;
   _ -> False}

eqb0 :: Int -> Int -> Bool
eqb0 x y =
  case eq_dec6 x y of {
   True -> True;
   False -> False}

flatten_e :: Enumeration -> [] Elt
flatten_e e =
  case e of {
   End -> [];
   More x t r -> ( : ) x (app (elements t) (flatten_e r))}

data R_bal =
   R_bal_0 T1 Int T1
 | R_bal_1 T1 Int T1 T Tree Int Tree
 | R_bal_2 T1 Int T1 T Tree Int Tree
 | R_bal_3 T1 Int T1 T Tree Int Tree T Tree Int Tree
 | R_bal_4 T1 Int T1
 | R_bal_5 T1 Int T1 T Tree Int Tree
 | R_bal_6 T1 Int T1 T Tree Int Tree
 | R_bal_7 T1 Int T1 T Tree Int Tree T Tree Int Tree
 | R_bal_8 T1 Int T1

data R_remove_min =
   R_remove_min_0 Tree Elt T1
 | R_remove_min_1 Tree Elt T1 T Tree Int Tree (( , ) T1 Elt) R_remove_min
 T1 Elt

data R_merge =
   R_merge_0 Tree Tree
 | R_merge_1 Tree Tree T Tree Int Tree
 | R_merge_2 Tree Tree T Tree Int Tree T Tree Int Tree T1 Elt

data R_concat =
   R_concat_0 Tree Tree
 | R_concat_1 Tree Tree T Tree Int Tree
 | R_concat_2 Tree Tree T Tree Int Tree T Tree Int Tree T1 Elt

data R_inter =
   R_inter_0 Tree Tree
 | R_inter_1 Tree Tree T Tree Int Tree
 | R_inter_2 Tree Tree T Tree Int Tree T Tree Int Tree T1 Bool T1 Tree
 R_inter Tree R_inter
 | R_inter_3 Tree Tree T Tree Int Tree T Tree Int Tree T1 Bool T1 Tree
 R_inter Tree R_inter

data R_diff =
   R_diff_0 Tree Tree
 | R_diff_1 Tree Tree T Tree Int Tree
 | R_diff_2 Tree Tree T Tree Int Tree T Tree Int Tree T1 Bool T1 Tree
 R_diff Tree R_diff
 | R_diff_3 Tree Tree T Tree Int Tree T Tree Int Tree T1 Bool T1 Tree
 R_diff Tree R_diff

data R_union =
   R_union_0 Tree Tree
 | R_union_1 Tree Tree T Tree Int Tree
 | R_union_2 Tree Tree T Tree Int Tree T Tree Int Tree T1 Bool T1 Tree
 R_union Tree R_union

type T6 = Int

compare9 :: Int -> Int -> Comparison
compare9 x y =
  case compare2 x y of {
   LT -> Lt;
   EQ -> Eq;
   GT -> Gt}

eq_dec7 :: Int -> Int -> Bool
eq_dec7 =
  eq_dec0

type Elt0 = Int

type T_ = T1
  -- singleton inductive, whose constructor was Mkt

this :: T_ -> T1
this t =
  t

type T7 = T_

mem0 :: Elt0 -> T7 -> Bool
mem0 x s =
  mem x (this s)

add5 :: Elt0 -> T7 -> T7
add5 x s =
  add4 x (this s)

remove0 :: Elt0 -> T7 -> T7
remove0 x s =
  remove x (this s)

singleton0 :: Elt0 -> T7
singleton0 =
  singleton

union0 :: T7 -> T7 -> T7
union0 s s' =
  union (this s) (this s')

inter0 :: T7 -> T7 -> T7
inter0 s s' =
  inter (this s) (this s')

diff0 :: T7 -> T7 -> T7
diff0 s s' =
  diff (this s) (this s')

equal0 :: T7 -> T7 -> Bool
equal0 s s' =
  equal (this s) (this s')

subset0 :: T7 -> T7 -> Bool
subset0 s s' =
  subset (this s) (this s')

empty0 :: T7
empty0 =
  empty

is_empty0 :: T7 -> Bool
is_empty0 s =
  is_empty (this s)

elements0 :: T7 -> [] Elt0
elements0 s =
  elements (this s)

choose0 :: T7 -> Maybe Elt0
choose0 s =
  choose (this s)

fold0 :: (Elt0 -> a1 -> a1) -> T7 -> a1 -> a1
fold0 f s =
  fold f (this s)

cardinal0 :: T7 -> Int
cardinal0 s =
  cardinal (this s)

filter0 :: (Elt0 -> Bool) -> T7 -> T7
filter0 f s =
  filter f (this s)

for_all0 :: (Elt0 -> Bool) -> T7 -> Bool
for_all0 f s =
  for_all f (this s)

exists_0 :: (Elt0 -> Bool) -> T7 -> Bool
exists_0 f s =
  exists_ f (this s)

partition0 :: (Elt0 -> Bool) -> T7 -> ( , ) T7 T7
partition0 f s =
  let {p = partition f (this s)} in (,) (fst p) (snd p)

eq_dec8 :: T7 -> T7 -> Bool
eq_dec8 s0 s'0 =
  let {b = equal s0 s'0} in case b of {
                             True -> True;
                             False -> False}

compare10 :: T7 -> T7 -> Comparison
compare10 s s' =
  compare4 (this s) (this s')

min_elt0 :: T7 -> Maybe Elt0
min_elt0 s =
  min_elt (this s)

max_elt0 :: T7 -> Maybe Elt0
max_elt0 s =
  max_elt (this s)

type F_env = Var -> Int

get_vars :: Exp -> T7
get_vars e =
  case e of {
   SKIP p -> singleton0 (fst p);
   X p -> singleton0 (fst p);
   CU p e' -> add5 (fst p) (get_vars e');
   RZ _ p -> singleton0 (fst p);
   RRZ _ p -> singleton0 (fst p);
   SR _ x -> singleton0 x;
   SRR _ x -> singleton0 x;
   Lshift x -> singleton0 x;
   Rshift x -> singleton0 x;
   Rev x -> singleton0 x;
   QFT x _ -> singleton0 x;
   RQFT x _ -> singleton0 x;
   Seq e1 e2 -> union0 (get_vars e1) (get_vars e2)}

x_var :: Int
x_var =
  0

y_var :: Int
y_var =
  succ 0

rz_adder' :: Var -> Int -> Int -> (Int -> Bool) -> Exp
rz_adder' x n size m =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> SKIP ((,) x 0))
    (\m0 -> Seq (rz_adder' x m0 size m)
    (case m m0 of {
      True -> SR (sub size n) x;
      False -> SKIP ((,) x m0)}))
    n

rz_adder :: Var -> Int -> (Int -> Bool) -> Exp
rz_adder x n m =
  rz_adder' x n n m

rz_sub' :: Var -> Int -> Int -> (Int -> Bool) -> Exp
rz_sub' x n size m =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> SKIP ((,) x 0))
    (\m0 -> Seq (rz_sub' x m0 size m)
    (case m m0 of {
      True -> SRR (sub size n) x;
      False -> SKIP ((,) x m0)}))
    n

rz_sub :: Var -> Int -> (Int -> Bool) -> Exp
rz_sub x n m =
  rz_sub' x n n m

rz_compare_half3 :: Var -> Int -> Posi -> (Int -> Bool) -> Exp
rz_compare_half3 x n c m =
  Seq (Seq (rz_sub x n m) (RQFT x n)) (cNOT ((,) x 0) c)

rz_moder' :: Int -> Int -> Var -> Var -> (Int -> Bool) -> Exp
rz_moder' i n x ex m =
  (\ fO fS n -> if n==0 then fO () else fS (n-1))
    (\_ -> SKIP ((,) x 0))
    (\j -> Seq (Seq (Seq (Seq (rz_compare_half3 x n ((,) ex j) m) (QFT x n)) (CU
    ((,) ex j) (rz_adder x n m))) (X ((,) ex j)))
    (rz_moder' j n x ex (cut_n (div_two_spec m) n)))
    i

rz_div_mod :: Int -> Var -> Var -> Int -> Exp
rz_div_mod n x ex m =
  let {i = findnum m (sub n (succ 0))} in
  Seq (Seq (Seq (Rev x) (QFT x n))
  (rz_moder' (succ i) n x ex
    (nat2fb (mul (pow (succ (succ 0)) i) m))))
  (inv_exp (Seq (Rev x) (QFT x n)))

rz_div_mod_out :: Int -> Int -> Exp
rz_div_mod_out size =
  rz_div_mod (succ size) x_var y_var

div_mod_vars :: Int -> T7
div_mod_vars n =
  get_vars (rz_div_mod_out n (succ 0))

div_mod_env :: Int -> F_env
div_mod_env n _ =
  succ n
