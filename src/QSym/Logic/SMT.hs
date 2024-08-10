{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-} -- needs to be set in any files that use this module for prettyprinting to work

module QSym.Logic.SMT
  (SMT
  ,Symbol
  ,symbol
  ,int
  ,bool
  ,getNames
  ,getBlockNames
  ,assert
  ,setOption

  ,varMap
  ,varMapBlock
  ,Decl

  ,Block
  -- block constructors
  ,one
  ,smtBlock

  -- boolean operations
  ,and'
  ,or'
  ,(^&&^)
  ,(^||^)

  ,pointsTo
  ,emp
  ,sep
  ,wand

  ,true
  ,false

  -- conditionals
  ,not'
  ,eq
  ,lt
  ,lte
  ,gt
  ,gte
  ,ifThenElse
  
  -- arithmetic operations
  ,add
  ,sub
  ,mul
  ,div
  ,store
  ,select

  ,smtMap
  ,smtMapList

  ,setLogic

  ,declareHeap
  ,declareConst
  ,declareConstList
  ,setInfo

  ,checkSAT
  ,getModel
  )
  where

import Prelude hiding (div)

import QSym.Logic.Syntax

import QSym.Utils (toLowerString)

import Prettyprinter hiding (sep)

import Data.Foldable
import Data.Coerce
import Data.String

data SExpr a
  = Atom a
  | List [SExpr a]
  -- | StringLit String
  | BoolLit Bool
  | IntLit Int
  deriving (Show, Functor, Foldable)

-- |SMT
data SMT a b
  = Decl (SExpr a)
  | Assert (SExpr a)
  | SExpr (SExpr a)
  deriving (Show)

data Symbol
data Decl
data Array i a

data SomeSMT a = forall b. SomeSMT (SMT a b)

getNames :: SMT a b -> [a]
getNames (Decl x) = toList x
getNames (Assert x) = toList x
getNames (SExpr x) = toList x

getBlockNames :: Block a -> [a]
getBlockNames (Block xs) = concatMap go xs
  where
    go (SomeSMT x) = getNames x

instance IsString a => IsString (SExpr a) where
  fromString = Atom . fromString

instance IsString a => IsString (SMT a b) where
  fromString = SExpr . fromString

instance IsString a => IsString (SomeSMT a) where
  fromString = SomeSMT . fromString

instance IsString a => Num (SMT a Int) where
  (+) = add
  (*) = mul
  (-) = sub
  fromInteger = int . fromInteger
  signum = error "SMT.signum"

symbol :: a -> SMT a b --Symbol
symbol = SExpr . Atom

int :: Int -> SMT a Int
int = SExpr . IntLit

bool :: Bool -> SMT a Bool
bool = SExpr . BoolLit

nil :: SMT a Symbol
nil = SExpr (IntLit 0)

newtype Block a = Block [SomeSMT a]
  deriving (Semigroup, Monoid)

varMap :: (a -> a') -> SMT a b -> SMT a' b
varMap f (Decl x) = Decl $ fmap f x
varMap f (Assert x) = Assert $ fmap f x
varMap f (SExpr x) = SExpr $ fmap f x

varMapBlock :: (a -> a') -> Block a -> Block a'
varMapBlock f (Block xs) = Block $ map go xs
  where
    go (SomeSMT x) = SomeSMT (varMap f x)

one :: SMT a b -> Block a
one x = Block [SomeSMT x]

smtBlock :: forall a b. [SMT a b] -> Block a
smtBlock = Block . map SomeSMT

-- NOTE: Do not export
apply :: a -> [SExpr a] -> SExpr a
apply f xs = List (Atom f : xs)

assert :: SMT a Bool -> SMT a Decl
assert (SExpr e) = Assert e

and' :: IsString a => [SMT a Bool] -> SMT a Bool
and' = SExpr . apply "and" . map toSExpr

or' :: IsString a => [SMT a Bool] -> SMT a Bool
or' = SExpr . apply "or" . map toSExpr

(^||^) :: IsString a => SMT a Bool -> SMT a Bool -> SMT a Bool
(^||^) x y = or' [x, y]

(^&&^) :: IsString a => SMT a Bool -> SMT a Bool -> SMT a Bool
(^&&^) x y = and' [x, y]

pointsTo :: IsString a => SMT a b -> SMT a b -> SMT a Bool
pointsTo lhs rhs = SExpr $ apply "pto" [toSExpr lhs, toSExpr rhs]

emp :: IsString a => SMT a Bool
emp = SExpr $ Atom "sep.emp"

sep :: IsString a => [SMT a Bool] -> SMT a Bool
sep = SExpr . apply "sep" . map toSExpr

wand :: IsString a => SMT a Bool -> SMT a Bool -> SMT a Bool
wand lhs rhs = SExpr (apply "wand" [toSExpr lhs, toSExpr rhs])

setLogic :: IsString a => a -> SMT a Decl
setLogic x = Decl $ apply "set-logic" [Atom x]

declareHeap :: IsString a => a -> a -> SMT a Decl
declareHeap lhs rhs = Decl $ apply "declare-heap" [Atom lhs, Atom rhs]

declareConst :: IsString a => a -> a -> SMT a Decl
declareConst name ty = Decl $ apply "declare-const" [Atom name, Atom ty]

declareConstList :: IsString a => [(a, a)] -> Block a
declareConstList = smtBlock . map (uncurry declareConst)

not' :: IsString a => SMT a Bool -> SMT a Bool
not' = SExpr . apply "not" . (:[]) . toSExpr

eq :: IsString a => SMT a b -> SMT a b -> SMT a Bool
eq x y = SExpr (apply "=" [toSExpr x, toSExpr y])

-- |lt (less than) corresponds to the SMT expression (< <lh> <rh>)
lt :: IsString a => SMT a b -> SMT a b -> SMT a Bool
lt x y = SExpr (apply "<" [toSExpr x, toSExpr y])

-- |lte (less than equal to) corresponds to the SMT expression (<= <lh> <rh>)
lte :: IsString a => SMT a b -> SMT a b -> SMT a Bool
lte x y = SExpr (apply "<=" [toSExpr x, toSExpr y])

-- |gt (greater than) corresponds to the SMT expression (> <lh> <rh>)
gt :: IsString a => SMT a b -> SMT a b -> SMT a Bool
gt x y = SExpr (apply ">" [toSExpr x, toSExpr y])

-- |gte (greater than equal) corresponds to the SMT expression (>= <lh> <rh>)
gte :: IsString a => SMT a b -> SMT a b -> SMT a Bool
gte x y = SExpr (apply ">=" [toSExpr x, toSExpr y])

ifThenElse :: IsString a => SMT a Bool -> SMT a b -> SMT a b -> SMT a b
ifThenElse c t f = SExpr (apply "ite" [toSExpr c, toSExpr t, toSExpr f])

add :: IsString a => SMT a Int -> SMT a Int -> SMT a Int
add x y = SExpr $ apply "+" [toSExpr x, toSExpr y]

sub :: IsString a => SMT a Int -> SMT a Int -> SMT a Int
sub x y = SExpr $ apply "-" [toSExpr x, toSExpr y]

mul :: IsString a => SMT a Int -> SMT a Int -> SMT a Int
mul x y = SExpr $ apply "*" [toSExpr x, toSExpr y]

div :: IsString a => SMT a Int -> SMT a Int -> SMT a Int
div x y = SExpr $ apply "/" [toSExpr x, toSExpr y]

true :: SMT a Bool
true = SExpr (BoolLit True)

false :: SMT a Bool
false = SExpr (BoolLit False)

updateIx :: IsString a => Int -> a -> a -> (SMT a b -> SMT a b) -> SMT a Bool
updateIx i oldName newName f =
  store i newName (f (symbol newName))
  -- SExpr $ apply "store" [Atom newName, IntLit i, at oldName i]

store :: IsString a => Int -> a -> SMT a b -> SMT a Bool
store i name v =
  SExpr $ apply "store" [Atom name, IntLit i, toSExpr v]

at' :: IsString a => SExpr a -> SExpr a -> SExpr a
at' arr i = apply "select" [arr, i]

select :: IsString a => SMT a (Array i b) -> SMT a i -> SMT a b
select arr i = SExpr (at' (toSExpr arr) (toSExpr i))

smtMap :: IsString a => Int -> (Int -> SMT a b -> SMT a b) -> a -> a -> SMT a Bool
smtMap size f oldName newName =
  and' (map go [0..size-1])
  where
    go i = updateIx i oldName newName (f i)

smtMapList :: IsString a => (SMT a b -> SMT a b) -> a -> a -> [SMT a b] -> SMT a Bool
smtMapList f oldName newName list =
  and' (zipWith go [0..] list)
  where
    go i v = store i newName (f v)

setInfo :: IsString a => [(a, a)] -> SMT a Decl
setInfo = Decl . apply "set-info" . map Atom . flatten
  where
    flatten :: [(a, a)] -> [a]
    flatten = concat . map (\(x, y) -> [x, y])

-- |setOption corresponds to the (set-option <keyword> <attr-value>) SMTLIB command
-- TODO: take in a list of tuples and return a Block a instead
setOption :: IsString a => a -> a -> SMT a Decl
setOption keyword attr_value = Decl $ apply "set-option" [Atom keyword, Atom attr_value]

checkSAT :: IsString a => SMT a Decl
checkSAT = Decl $ apply "check-sat" []

getModel :: IsString a => SMT a Decl
getModel = Decl $ apply "get-model" []

-- NOTE: Do not export
toSExpr :: SMT a b -> SExpr a
toSExpr (SExpr e) = e


instance Pretty a => Pretty (SExpr a) where
  pretty (Atom x) = pretty x
  pretty (List xs) = parens $ hsep $ map pretty xs
  pretty (BoolLit b) = pretty $ toLowerString $ show b -- SMTLIB v2 specifies booleans as lowercase keywords
  pretty (IntLit i) = pretty i

instance (IsString a, Pretty a) => Pretty (SMT a b) where
  pretty (Decl x) = pretty x
  pretty (Assert x) = pretty (List [Atom "assert", x])
  pretty (SExpr x) = pretty x

instance (IsString a, Pretty a) => Pretty (SomeSMT a) where
  pretty (SomeSMT x) = pretty x

instance (IsString a, Pretty a) => Pretty (Block a) where
  pretty (Block xs) = vcat $ map pretty xs

-- convertProp :: LExprProp -> SBool
-- convertProp (Prop xs) = foldr (.&&) sTrue (map convertConjunct xs)
--
-- convertConjunct :: LExprConjunct -> SBool
-- convertConjunct (PointsTo lhs rhs) = undefined
--
-- convertSteppedLocus :: SteppedLocus -> 
--
-- -- class ToSMT a where
-- --   toSMT :: a -> SBV a
--
--
--
