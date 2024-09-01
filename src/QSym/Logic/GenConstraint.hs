{-# LANGUAGE OverloadedStrings #-}

module QSym.Logic.GenConstraint
  (Name
  ,Verify (..)
  ,astSMT
  )
  where

import Prelude hiding (div)

import QSym.Logic.Syntax
import QSym.Logic.SMT
import QSym.Logic.Gen
-- import QSym.Logic.Linear

import Qafny.Syntax.AST hiding (Range (..), Block)
import qualified Qafny.Syntax.AST as Qafny
import Qafny.Syntax.Subst

import Data.Sum

import Data.Ord

import Control.Monad
import Control.Monad.Reader
import Data.String
import Data.List

import Prettyprinter

import Debug.Trace

astSMT :: Verify -> Int -> AST -> Block Name
astSMT verify bitSize ast =
  smtPreamble <> mkDeclarations block <> block <> smtCheck <> verifyEqs <> smtBlock [checkSAT, symbol "(get-unsat-core)"]
  where
    block = astConstraints bitSize ast

    smtCheck =
      smtBlock
        [checkSAT
        ,getModel
        -- ,symbol "(get-unsat-core)"
        ]

    verifyEqs :: Block Name
    verifyEqs =
      case verify of
        ExactValues initialState -> mconcat $ zipWith toMemEq [0..] initialState
        Satisfies prop -> prop (currentVar "mem") $ getLastMem block 

    toMemEq :: Int -> SMT Name Int -> Block Name
    toMemEq i v =
      smtBlock
        [assert $ eq (select (symbol (currentVar "mem")) (mkLoc i)) v]

astConstraints :: Int -> AST -> Block Name
astConstraints bitSize =
  mconcat . map (toplevelConstraints bitSize)

toplevelConstraints :: Int -> Toplevel () -> Block Name
toplevelConstraints bitSize (Toplevel (Inl qm)) =
  case qmBody qm of
    Nothing -> mempty
    Just block -> traceShow block $ runGen (blockListConstraints (reverse (inBlock block))) (buildEnv bitSize qm) -- TODO: Find a better way than reversing here

blockListConstraints :: [Stmt ()] -> Gen (Block Name)
blockListConstraints [] = pure mempty
blockListConstraints (x:xs) = do
  prop <- blockConstraints x
  rest <- mconcat <$> traverse (fmap (varMapBlock step) . blockConstraints) xs
  pure (prop <> rest)

blockConstraints :: Stmt () -> Gen (Block Name)
blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
blockConstraints (SCall f xs) = error "SCall"
blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement

-- TODO: Generalize to applying Hadamard to more than one location
blockConstraints (Partition [lhs] :*=: EHad) = do
  totalBits <- fmap envBitSize ask

  (physStart0, physEnd0) <- rangeToPhysicalIndices lhs

  let physStart = bvPosition physStart0
  let physEnd = bvPosition physEnd0

  let sizeAppliedTo = length [lhs]

  let possibleVecs = allPossibleBitVectors sizeAppliedTo

  let mkPossibility i vec = omega (bv2nat (bvGetRange i physStart physEnd) * bv2nat vec)
                                (int sizeAppliedTo)
                            * selectWithBitVector mem' vec

      possibilities i = map (mkPossibility i) possibleVecs

  pure $ smtBlock
    [ forAll "i" "Int" $
        eq (select mem (var "i"))
           (mul invSqrt2 (sum (possibilities (int2bv totalBits (var "i")))))
    ]
  where
    mem = symbol (currentVar "mem")
    mem' = symbol (step (currentVar "mem"))

  -- let usedInput = partitionToName lhs
  -- otherInputs <- getOtherInputs [usedInput]
  --
  -- totalQubits <- envBitSize <$> ask
  --
  -- let resized = resizeGate totalQubits usedInput hadamard
  -- let applied = applySMTMatrix totalQubits (currentVar "mem") (step (currentVar "mem")) resized
  --
  -- traceShow resized $ pure $ applied
blockConstraints (SDafny _) = pure mempty
blockConstraints (SIf (GEPartition part Nothing) part' (Qafny.Block [x :*=: ELambda (LambdaF { eBases = [EOp2 OMod (EOp2 OAdd (EVar v) (ENum 1)) (ENum 2)] })])) = do
    undefined
  -- totalQubits <- envBitSize <$> ask
  --
  --   -- TODO: Change hardcoded 0 to proper input index
  -- let resized = resizeGate totalQubits 0 cnot
  -- let block = applySMTMatrix totalQubits (currentVar "mem") (step (currentVar "mem")) resized
  --
  -- pure block
blockConstraints s = error $ "unimplemented: " ++ show s

allPossibleBitVectors :: IsString a => Int -> [BitVector a]
allPossibleBitVectors size = map (int2bv size . int) [0 .. 2 ^ size]

-- allPossibleBitSeqs :: Int -> [[Int]]
-- allPossibleBitSeqs size = replicateM size [0, 1]
--
-- bitSeqToInt :: [Int] -> Int
-- bitSeqToInt [] = 0
-- bitSeqToInt (b:bs) = b + 2 * bitSeqToInt bs

applyLambda :: LambdaF (Exp ()) -> Exp () -> Exp ()
applyLambda (LambdaF { bBases = [paramVar], eBases = [body] }) arg =
  subst [(paramVar, arg)] body

-- toLocus :: Partition -> Locus
-- toLocus (Partition xs) = Locus $ map convertRange xs
--
-- convertRange :: Qafny.Range -> Range
-- convertRange (Qafny.Range x start end) = Range x (convertSimpleExpr start) (convertSimpleExpr end)

-- convertLambda :: LambdaF (Exp ()) -> SteppedLocus -> Int -> HighLevelSMT Int
-- convertLambda (LambdaF { bBases = [paramVar], eBases = [body] }) locus ix =
--   convertExpr body (Just (paramVar, select (symbol (LocusName locus)) (int ix)))

-- convertSimpleExpr :: Exp () -> SimpleExpr
-- convertSimpleExpr (ENum i) = Lit i
-- convertSimpleExpr (EVar x) = Var x
-- convertSimpleExpr (EOp2 OAdd x y) = Add (convertSimpleExpr x) (convertSimpleExpr y)

data Verify
  = ExactValues [SMT Name Int]
  | Satisfies (Name -> -- Input
               Name -> -- Output
               Block Name)

-- type SMTMatrix = Matrix (SMT Name Int)
--
-- data Gate =
--   Gate
--     { gateNumInputs :: Int
--     , gateNumOutputs :: Int
--     , gateMap :: SMTMatrix -- TODO: Use a better representation?
--     }
--   deriving (Show)
--
-- resizeGate :: Int -> Int -> Gate -> SMTMatrix
-- resizeGate totalQubits inputIndex gate =
--     trace ("totalQubits = " ++ show totalQubits) $
--     tensor' (identity inputIndex)
--             (tensor' (gateMap gate)
--                      (identity (2 * (totalQubits - gateNumOutputs gate))))
--   where
--     tensor' [] x = x
--     tensor' x [] = x
--     tensor' x y = tensor x y
--
-- applySMTMatrix :: Int -> Name -> Name -> SMTMatrix -> Block Name
-- applySMTMatrix totalQubits oldName newName mat =
--   let newMem = multColumn mat (getMemoryVector size oldName)
--   in
--   smtBlock $ zipWith go [0..size] newMem
--   where
--     size = totalQubits*2
--     go i v =
--       assert $ eq (select (symbol newName) (int i)) v
--
-- getMemoryVector :: Int -> Name -> [SMT Name Int]
-- getMemoryVector totalQubits mem = map (select (symbol mem) . int) [0..totalQubits-1]

heapType :: String
heapType = "(Array Int Real)"
-- heapType = "(Array Int (Array Int Real))"

bitVecSize :: Int
bitVecSize = 4

bitVecType :: String
bitVecType = "(_ BitVec " ++ show bitVecSize ++ ")"

bitVecArrayType :: String
bitVecArrayType = "(Array Int " ++ bitVecType ++ ")"

bitVecLit :: String -> SMT Name Int
bitVecLit i = fromString $ "(_ bv" ++ i ++ " " ++ show bitVecSize ++ ")"

mkLoc :: Int -> SMT Name a
mkLoc i = fromString (show i) --fromString $ "q" ++ show i

getLastMem :: Block Name -> Name
getLastMem block =
  let blockNames = getBlockNames block
  in VarName . maximumBy (comparing steppedToInt) . nub $ getSteppedVar "mem" blockNames

mkDeclarations :: Block Name -> Block Name
mkDeclarations block =
  let blockNames = getBlockNames block
      memNames = map VarName . nub $ getSteppedVar "mem" blockNames
      memVecNames = map VarName . nub $ getSteppedVar "mem-vecs" blockNames
  in
  declareConstList (zip memNames (repeat (fromString heapType)))
    <>
  declareConstList (zip memVecNames (repeat (fromString (fromString bitVecArrayType))))

--
-- astSMT :: Verify -> Int -> AST -> Block Name
-- astSMT verify bitSize ast =
--   smtPreamble <> mkDeclarations block <> block <> smtCheck <> verifyEqs <> smtBlock [checkSAT, symbol "(get-unsat-core)"]
--   where
--     block = astConstraints bitSize ast
--
--     smtCheck =
--       smtBlock
--         [checkSAT
--         ,getModel
--         -- ,symbol "(get-unsat-core)"
--         ]
--
--     verifyEqs :: Block Name
--     verifyEqs =
--       case verify of
--         ExactValues initialState -> mconcat $ zipWith toMemEq [0..] initialState
--         Satisfies prop -> prop (currentVar "mem") $ getLastMem block 
--
--     toMemEq :: Int -> SMT Name Int -> Block Name
--     toMemEq i v =
--       smtBlock
--         [assert $ eq (select (symbol (currentVar "mem")) (mkLoc i)) v]
--
-- astConstraints :: Int -> AST -> Block Name
-- astConstraints bitSize =
--   mconcat . map (toplevelConstraints bitSize)
--
-- toplevelConstraints :: Int -> Toplevel () -> Block Name
-- toplevelConstraints bitSize (Toplevel (Inl qm)) =
--   case qmBody qm of
--     Nothing -> mempty
--     Just block -> traceShow block $ runGen (blockListConstraints (reverse (inBlock block))) (buildEnv bitSize qm) -- TODO: Find a better way than reversing here
--
-- blockListConstraints :: [Stmt ()] -> Gen (Block Name)
-- blockListConstraints [] = pure mempty
-- blockListConstraints (x:xs) = do
--   prop <- blockConstraints x
--   rest <- mconcat <$> traverse (fmap (varMapBlock step) . blockConstraints) xs
--   pure (prop <> rest)
--
-- blockConstraints :: Stmt () -> Gen (Block Name)
-- blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
-- blockConstraints (SCall f xs) = error "SCall"
-- blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
-- blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement
-- blockConstraints (lhs :*=: EHad) = do
--   let usedInput = partitionToName lhs
--   otherInputs <- getOtherInputs [usedInput]
--
--   totalQubits <- envBitSize <$> ask
--
--   let resized = resizeGate totalQubits usedInput hadamard
--   let applied = applySMTMatrix totalQubits (currentVar "mem") (step (currentVar "mem")) resized
--
--   traceShow resized $ pure $ applied
-- blockConstraints (SDafny _) = pure mempty
-- blockConstraints (SIf (GEPartition part Nothing) part' (Qafny.Block [x :*=: ELambda (LambdaF { eBases = [EOp2 OMod (EOp2 OAdd (EVar v) (ENum 1)) (ENum 2)] })])) = do
--   totalQubits <- envBitSize <$> ask
--
--     -- TODO: Change hardcoded 0 to proper input index
--   let resized = resizeGate totalQubits 0 cnot
--   let block = applySMTMatrix totalQubits (currentVar "mem") (step (currentVar "mem")) resized
--
--   pure block
-- blockConstraints s = error $ "unimplemented: " ++ show s
--
-- applyLambda :: LambdaF (Exp ()) -> Exp () -> Exp ()
-- applyLambda (LambdaF { bBases = [paramVar], eBases = [body] }) arg =
--   subst [(paramVar, arg)] body
--
-- toLocus :: Partition -> Locus
-- toLocus (Partition xs) = Locus $ map convertRange xs
--
-- -- TODO: Partially hardcoded for now
-- partitionToName :: Partition -> Int
-- partitionToName (Partition [Qafny.Range x start end]) = evalToInt start
--
-- evalToInt :: Exp () -> Int
-- evalToInt (ENum i) = i
--
-- toLocusExpr :: Partition -> HighLevelSMT Int
-- toLocusExpr = symbol . LocusName . Current . toLocus

-- -- |convertExpr converts a Qafny AST Expression to a SimpleExpr that can be used for symbolic execution
-- convertExpr :: Exp () -> Maybe (Var, HighLevelSMT Int) -> HighLevelSMT Int
-- convertExpr e0 substMaybe = go e0
--   where
--     go (ENum i) = int i
--     go (EVar x) =
--       case substMaybe of
--         Just (paramVar, arg)
--           | x == paramVar -> arg
--         _ -> symbol (VarName (Current x))
--     go (EOp2 OAdd x y) = add (go x) (go y)
--
-- convertBoolExpr :: Exp () -> HighLevelSMT Bool
-- convertBoolExpr (ENum i) = bool (i /= 0)
-- convertBoolExpr (EVar x) = symbol (VarName (Current x))
--
-- convertGuardExp :: GuardExp -> HighLevelSMT Bool
-- convertGuardExp (GEPartition p eMaybe) =
--     not' $ eq (symbol (LocusName (Current (toLocus p)))) (int 0)
--   where
--     eBool =
--       case eMaybe of
--         Just e -> convertBoolExpr e
--         Nothing -> true
--
-- lookupCell :: Name -> Int -> Int -> SMT Name Int
-- lookupCell name i j =
--   select (select (symbol name) (int i)) (mkLoc j)
--
-- cnot :: Gate
-- cnot = controlled notGate
--   -- Gate
--   --   { gateNumInputs = 2
--   --   , gateNumOutputs = 2
--   --   , gateMap =
--   --       [[1, 0, 0, 0]
--   --       ,[0, 1, 0, 0]
--   --       ,[0, 0, 0, 1]
--   --       ,[0, 0, 1, 0]]
--   --   }
--
-- hadamard :: Gate
-- hadamard =
--   Gate
--     { gateNumInputs = 1
--     , gateNumOutputs = 1
--     , gateMap =
--         scalarMult (1 `div` "sqrt2")
--           [[1, 1]
--           ,[1, -1]]
--     }
--
-- notGate :: Gate
-- notGate =
--   Gate
--     { gateNumInputs = 1
--     , gateNumOutputs = 1
--     , gateMap =
--         [[0, 1]
--         ,[1, 0]]
--     }
--
-- -- Controlled version of a 1-qubit gate
-- controlled :: Gate -> Gate
-- controlled g =
--   Gate
--     { gateNumInputs = 1 + gateNumInputs g
--     , gateNumOutputs = 1 + gateNumOutputs g
--     , gateMap =
--         flattenMatrix
--           [ [identity 2, zeroes 2]
--           , [zeroes 2,   gateMap g]
--           ]
--     }
--
-- unchanged :: Name -> SMT Name Decl
-- unchanged name =
--   update name id
--
-- update :: Name -> (SMT Name a -> SMT Name a) -> SMT Name Decl
-- update name f =
--   assert $ eq (symbol (step name)) (f (symbol name))
--
-- -- convertExpr env (ENum i) = int i
-- -- convertExpr env (EVar x) = symbol (VarName (Current x))
-- -- convertExpr env (EOp2 OAdd x y) = add (convertExpr x) (convertExpr y)
--
-- -- -- |convertApplyExpr converts a Qafny AST Expression used in an apply statement "*=" 
-- -- --  to a lambda that can be used in the LApply constructor.
-- -- --  where:
-- -- --    name of type `String` is the name to use for Hadamard gates and similar.
-- -- --    expression of type `Exp ()` is the expression tree to convert
-- -- --  return of type `SimpleExpr`
-- -- convertApplyExpr :: String -> Exp () -> HighLevelSMT Int
-- -- convertApplyExpr name EHad = Hadamard name
--
