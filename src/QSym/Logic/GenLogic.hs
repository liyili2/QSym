module QSym.Logic.GenLogic
  where

import Prelude hiding (div)

import QSym.Logic.Syntax hiding (Add)
import QSym.Logic.SMT
import QSym.Logic.Gen

import Qafny.Syntax.AST hiding (Range (..), Block, Var)
import qualified Qafny.Syntax.AST as Qafny
import Qafny.Syntax.Subst

import QSym.Utils
import QSym.ASTLogic

import Data.Sum

import Data.Ord

import Control.Monad.Reader
import Data.String
import Data.List

import Prettyprinter

data SMTVar = IxVar Var | NameVar String

instance IsString SMTVar where
  fromString = NameVar

astSMT :: Int -> AST -> Block SMTVar
astSMT bitSize = predToSMT . toPred . astToQafnyExpr

predToSMT :: Pred -> Block SMTVar
predToSMT = undefined

arithToSMT :: ArithExpr -> SMT SMTVar Int
arithToSMT (AddrVar x) = symbol (IxVar x)
arithToSMT (HeapVar x) = symbol (IxVar x)
arithToSMT (StateVal st) = error "arithToSMT: StateVal"
arithToSMT (Scal x) = double x
arithToSMT (Add x y) = add (arithToSMT x) (arithToSMT y)
arithToSMT (Sub x y) = sub (arithToSMT x) (arithToSMT y)
arithToSMT (Mult x y) = mul (arithToSMT x) (arithToSMT y)
arithToSMT (Div x y) = div (arithToSMT x) (arithToSMT y)

toPred :: [QafnyExpr] -> Pred
toPred = go mempty true
  where
    true = Equal (Scal 0) (Scal 0) -- TODO: Get rid of this

    go lmap p [] = p
    go lmap p (e:es) =
      let (p', lmap') = interpret lmap p e
      in
      go lmap' p' es

astToQafnyExpr :: AST -> [QafnyExpr]
astToQafnyExpr (Toplevel (Inl qm)) =
  case qmBody qm of
    Nothing -> mempty
    Just block ->
      map blockListConstraints (inBlock block)

blockConstraints :: Stmt () -> [QafnyExpr]
blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
blockConstraints (SCall f xs) = error "SCall"
blockConstraints (SVar {}) = error "SVar" -- TODO: Implement
blockConstraints (_ ::=: _) = error "::=:" -- TODO: Implement
blockConstraints (lhs :*=: EHad) = error "EHad" -- TODO: Implement
blockConstraints (SDafny _) = pure mempty
blockConstraints (SIf (GEPartition part Nothing) part' (Qafny.Block [x :*=: ELambda (LambdaF { eBases = [EOp2 OMod (EOp2 OAdd (EVar v) (ENum 1)) (ENum 2)] })])) =
      [Apply (toLocus x) (Add ]

blockConstraints s = error $ "unimplemented: " ++ show s

toLocus :: Partition -> Locus
toLocus (Partition xs) = Locus $ map convertRange xs

-- genLogic :: Int -> AST -> [Pred]
-- genLogic bitSize ast = undefined
--   where
--     block = astConstraints bitSize ast
--
-- astConstraints :: Int -> AST -> [Pred]
-- astConstraints bitSize =
--   mconcat . map (toplevelConstraints bitSize)
--
-- toplevelConstraints :: Int -> Toplevel () -> [Pred]
-- toplevelConstraints bitSize (Toplevel (Inl qm)) =
--   case qmBody qm of
--     Nothing -> mempty
--     Just block -> runGen (blockListConstraints (inBlock block)) (buildEnv bitSize qm)
--
-- blockListConstraints :: [Stmt ()] -> Gen [Pred]
-- blockListConstraints [] = pure mempty
-- blockListConstraints (x:xs) = do
--   prop <- blockConstraints x
--   rest <- mconcat <$> traverse (fmap (map step) . blockConstraints) xs
--   pure (prop <> rest)
--
-- blockConstraints :: Stmt () -> Gen [Pred]
-- blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
-- blockConstraints (SCall f xs) = error "SCall"
-- blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
-- blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement
-- blockConstraints (lhs :*=: EHad) = do
--   undefined
-- blockConstraints (SDafny _) = pure mempty
-- blockConstraints (SIf (GEPartition part Nothing) part' (Qafny.Block [x :*=: ELambda (LambdaF { eBases = [EOp2 OMod (EOp2 OAdd (EVar v) (ENum 1)) (ENum 2)] })])) = do
--   pure $ cnot (partitionToName part) (partitionToName x) (currentVar "mem") (currentVar "mem-vecs")
--
--   -- bodyConstraints <- blockListConstraints (inBlock body)
--   --
--   -- let cond' = convertGuardExp cond
--   --
--   -- pure $ ifThenElse cond'
--   --           bodyConstraints
--   --           undefined --(and' (map unchanged (getNames bodyConstraints)))
--   --
--   -- -- pure undefined -- $ Prop [If (LSimpleExpr (fromGuard cond)) undefined]
--   -- where
--   --   fromGuard (GEPartition p Nothing) = toLocusExpr p
--   --   -- TODO: Is this right?
--   --   fromGuard (GEPartition _ (Just e)) = convertExpr e Nothing
-- blockConstraints s = error $ "unimplemented: " ++ show s
--
-- -- blockConstraints :: Stmt () -> Gen (Block Name)
-- -- blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
-- -- blockConstraints (SCall f xs) = error "SCall"
-- -- blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
-- -- blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement
-- -- blockConstraints (lhs :*=: EHad) = do
-- --   let usedInput = partitionToName lhs
-- --   otherInputs <- getOtherInputs [usedInput]
-- --   pure $ hadamard usedInput otherInputs (currentVar "mem")
-- -- -- blockConstraints (lhs :*=: rhs@(ELambda lam)) = do
-- --   -- -- Add an apply constraint to the locus that was modified
-- --   -- -- and unchanged constraints to the others
-- --   --
-- --   -- -- create the apply expression
-- --   -- let locus = toLocus lhs
-- --   -- -- let apply_expr = undefined --LApply (Current locus) ("x" :=> (convertApplyExpr "x" rhs))
-- --   -- -- For BellPair, all we have to do is inverse the locus TODO: actually get all the variables in the environment
-- --   -- -- read the env (contains the function inputs and outputs, needed for the unchanged prop)
-- --   -- env <- ask
-- --   -- let bitSize = envBitSize env
-- --   --
-- --   -- pure $ smtMap bitSize (\i v -> convertLambda lam (Current locus) i)
-- --   --          (LocusName (Step (Current locus))) (LocusName (Current locus))
-- --   --
-- --   -- -- pure $ smtMap bitSize (\i v -> convertExpr [(locus, i)] v)
-- --   -- --          (LocusName (Step (Current locus))) (LocusName (Current locus))
-- --   --
-- --   -- -- pure $ smtMapList (\i -> undefined)
-- --   -- --          (LocusName (Step (Current locus))) (LocusName (Current locus))
-- --   -- --          (map int [0..bitSize-1])
-- --   --
-- --   -- -- add the unchanged properties
-- --   -- -- let unchanged = enumerateUnchanged (allBindings env) locus
-- --   -- -- pure undefined -- $ Prop ([Unchanged (Step (Current (invLocus locus)))] ++ [PointsTo (Step (Current locus)) apply_expr])
-- -- blockConstraints (SDafny _) = pure mempty
-- -- blockConstraints (SIf (GEPartition part Nothing) part' (Qafny.Block [x :*=: ELambda (LambdaF { eBases = [EOp2 OMod (EOp2 OAdd (EVar v) (ENum 1)) (ENum 2)] })])) = do
-- --     pure $ cnot (partitionToName part) (partitionToName x) (currentVar "mem") (currentVar "mem-vecs")
-- --
-- --   -- bodyConstraints <- blockListConstraints (inBlock body)
-- --   --
-- --   -- let cond' = convertGuardExp cond
-- --   --
-- --   -- pure $ ifThenElse cond'
-- --   --           bodyConstraints
-- --   --           undefined --(and' (map unchanged (getNames bodyConstraints)))
-- --   --
-- --   -- -- pure undefined -- $ Prop [If (LSimpleExpr (fromGuard cond)) undefined]
-- --   -- where
-- --   --   fromGuard (GEPartition p Nothing) = toLocusExpr p
-- --   --   -- TODO: Is this right?
-- --   --   fromGuard (GEPartition _ (Just e)) = convertExpr e Nothing
-- -- blockConstraints s = error $ "unimplemented: " ++ show s
-- --
-- -- -- unchanged :: Name -> HighLevelSMT Bool
-- -- -- unchanged x = eq (symbol x) (symbol (step x))
-- --
-- -- applyLambda :: LambdaF (Exp ()) -> Exp () -> Exp ()
-- -- applyLambda (LambdaF { bBases = [paramVar], eBases = [body] }) arg =
-- --   subst [(paramVar, arg)] body
-- --
-- -- toLocus :: Partition -> Locus
-- -- toLocus (Partition xs) = Locus $ map convertRange xs
--
-- -- TODO: Partially hardcoded for now
-- partitionToName :: Partition -> Int
-- partitionToName (Partition [Qafny.Range x start end]) = evalToInt start
--
-- -- evalToInt :: Exp () -> Int
-- -- evalToInt (ENum i) = i
-- --
-- -- toLocusExpr :: Partition -> HighLevelSMT Int
-- -- toLocusExpr = symbol . LocusName . Current . toLocus
-- --
-- -- convertRange :: Qafny.Range -> Range
-- -- convertRange (Qafny.Range x start end) = Range x (convertSimpleExpr start) (convertSimpleExpr end)
-- --
-- -- convertLambda :: LambdaF (Exp ()) -> SteppedLocus -> Int -> HighLevelSMT Int
-- -- convertLambda (LambdaF { bBases = [paramVar], eBases = [body] }) locus ix =
-- --   convertExpr body (Just (paramVar, select (symbol (LocusName locus)) (int ix)))
-- --
-- -- convertSimpleExpr :: Exp () -> SimpleExpr
-- -- convertSimpleExpr (ENum i) = Lit i
-- -- convertSimpleExpr (EVar x) = Var x
-- -- convertSimpleExpr (EOp2 OAdd x y) = Add (convertSimpleExpr x) (convertSimpleExpr y)
-- --
-- -- -- |convertExpr converts a Qafny AST Expression to a SimpleExpr that can be used for symbolic execution
-- -- convertExpr :: Exp () -> Maybe (Var, HighLevelSMT Int) -> HighLevelSMT Int
-- -- convertExpr e0 substMaybe = go e0
-- --   where
-- --     go (ENum i) = int i
-- --     go (EVar x) =
-- --       case substMaybe of
-- --         Just (paramVar, arg)
-- --           | x == paramVar -> arg
-- --         _ -> symbol (VarName (Current x))
-- --     go (EOp2 OAdd x y) = add (go x) (go y)
-- --
-- -- convertBoolExpr :: Exp () -> HighLevelSMT Bool
-- -- convertBoolExpr (ENum i) = bool (i /= 0)
-- -- convertBoolExpr (EVar x) = symbol (VarName (Current x))
-- --
-- -- convertGuardExp :: GuardExp -> HighLevelSMT Bool
-- -- convertGuardExp (GEPartition p eMaybe) =
-- --     not' $ eq (symbol (LocusName (Current (toLocus p)))) (int 0)
-- --   where
-- --     eBool =
-- --       case eMaybe of
-- --         Just e -> convertBoolExpr e
-- --         Nothing -> true
--
-- lookupCell :: Name -> Int -> Int -> SMT Name Int
-- lookupCell name i j =
--   select (select (symbol name) (int i)) (mkLoc j)
--
-- cnot :: Int -> Int -> Name -> Name -> Block Name
-- cnot i j mem memVecs =
--   smtBlock
--     [assert $ eq (lookupCell (step mem) i 0)
--                  (lookupCell mem i 0)
--     ,assert $ eq (lookupCell (step mem) i 1)
--                  (lookupCell mem i 1)
--
--     ,assert $ eq (lookupCell (step mem) (i+1) 0)
--                  (lookupCell mem (i+1) 1)
--     ,assert $ eq (lookupCell (step mem) (i+1) 1)
--                  (lookupCell mem (i+1) 0)
--     -- [assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 0))
--     --              (select (select (symbol mem) (mkLoc i)) (int 0))
--     -- ,assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 1))
--     --              (select (select (symbol mem) (mkLoc i)) (int 1))
--     --
--     -- ,assert $ eq (select (select (symbol (step mem)) (mkLoc (i+1))) (int 0))
--     --              (select (select (symbol mem) (mkLoc (i+1))) (int 0))
--     -- ,assert $ eq (select (select (symbol (step mem)) (mkLoc (i+1))) (int 1))
--     --              (select (select (symbol mem) (mkLoc (i+1))) (int 1))
--     --
--     -- entangle
--     -- TODO: Hardcoded right now
--     -- ,assert $ eq (select (symbol memVecs) (int 0)) $ bitVecLit "00"
--     -- ,assert $ eq (select (symbol memVecs) (int 0)) $ bitVecLit "11"
--     ]
--
-- hadamard :: Int -> [Int] -> Name -> Block Name
-- hadamard i otherInputs mem =
--   smtBlock $
--     [assert $ eq (lookupCell (step mem) i 0) (hadamardFirst 0 mem)
--     ,assert $ eq (lookupCell (step mem) i 1) (hadamardSecond 0 mem)
--     -- [assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 0)) (hadamardFirst 0 mem)
--     -- ,assert $ eq (select (select (symbol (step mem)) (mkLoc i)) (int 1)) (hadamardSecond 1 mem)
--     ] ++ concatMap handleOtherInput otherInputs
--     where
--       handleOtherInput j =
--         [assert $ eq (lookupCell (step mem) j 0)
--                      (lookupCell mem j 0)
--         ,assert $ eq (lookupCell (step mem) j 1)
--                      (lookupCell mem j 1)
--         ]
--
-- hadamardFirst :: Int -> Name -> SMT Name Int
-- hadamardFirst loc mem =
--   div
--     (add (select (select (symbol mem) (int loc)) (int 0))
--          (select (select (symbol mem) (int loc)) (int 1)))
--     "sqrt2"
--
-- hadamardSecond :: Int -> Name -> SMT Name Int
-- hadamardSecond loc mem =
--   div
--     (sub (select (select (symbol mem) (int loc)) (int 0))
--          (select (select (symbol mem) (int loc)) (int 1)))
--     "sqrt2"
--
-- unchanged :: Name -> SMT Name Decl
-- unchanged name =
--   update name id
--
