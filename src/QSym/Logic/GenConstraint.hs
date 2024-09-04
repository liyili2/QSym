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
import QSym.Logic.Name
import QSym.Logic.Gen
import QSym.Logic.Memory
import QSym.Logic.Operation
import QSym.Logic.Builtins
-- import QSym.Logic.Linear

import Qafny.Syntax.AST hiding (Range (..), Block)
import qualified Qafny.Syntax.AST as Qafny
import Qafny.Syntax.Subst

import Data.Sum

import Data.Ord

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.String
import Data.List

import Prettyprinter

import Debug.Trace

astSMT :: Verify -> Int -> AST -> Block Name
astSMT verify bitSize ast =
  smtPreamble <> mkDeclarations block <> block <> smtCheck <> smtBlock [checkSAT, symbol "(get-unsat-core)"]
  where
    block = astConstraints verifyEqs bitSize ast

    smtCheck =
      smtBlock
        [checkSAT
        ,getModel
        -- ,symbol "(get-unsat-core)"
        ]

    verifyEqs :: VerifySatisfies
    verifyEqs =
      case verify of
            -- TODO: Implement this for testing specific values
        -- ExactValues initialState -> mconcat $ zipWith toMemEq [0..] initialState
        Satisfies prop -> prop --(currentVar "mem") $ getLastMem block 
        _ -> \_ _ -> pure mempty

    toMemEq :: Int -> SMT Name Int -> Block Name
    toMemEq i v =
      smtBlock
        [assert $ eq (select (symbol (currentVar "mem")) (mkLoc i)) v]

-- getQubit :: String -> Int -> Gen (SMT Name Int)
-- getQubit var varIx = undefined

-- getTotalProbForVar :: String -> Int -> BitVector Name -> SMT Name (Array Int Int) -> Gen (SMT Name Int)
-- getTotalProbForVar var shift bitVec mem = do
--   totalBits <- fmap envBitSize ask
--
--   base <- getVarBaseIndex var
--
--   let prefixSize = shift + base
--       postfixSize = totalBits - prefixSize - bitVectorSize bitVec
--
--       allPrefixes = allPossibleBitVectors prefixSize
--       allPostfixes = allPossibleBitVectors postfixSize
--       allCircumfixes = do
--         prefix <- allPrefixes
--         postfix <- allPostfixes
--         pure (prefix, postfix)
--
--       paste (pre, post) = bvConcat [pre, bitVec, post]
--
--       allPossibilities = map paste allCircumfixes
--
--   pure $ sum (map (selectWithBitVector mem) allPossibilities)

astConstraints :: VerifySatisfies -> Int -> AST -> Block Name
astConstraints verify bitSize =
  mconcat . map (toplevelConstraints verify bitSize)

initialMemory :: Int -> Memory
initialMemory bitSize =
  mkMemory
    (EN [0])
    bitSize
    (currentVar "mem-amp")
    (currentVar "mem-phase")
    (currentVar "mem-bit-vec")

toplevelConstraints :: VerifySatisfies -> Int -> Toplevel () -> Block Name
toplevelConstraints verify bitSize (Toplevel (Inl qm)) =
  case qmBody qm of
    Nothing -> mempty
    Just block ->
      let initialDecls = declareMemory bitSize (initialMemory bitSize)

          go = do --mainPart <- blockListConstraints (reverse (inBlock block)) -- TODO: Find a better way than reversing here
                  mainPart <- blockListConstraints (inBlock block)
                  fmap ((initialDecls <> mainPart) <>) (verify (currentVar "mem") (getLastMem mainPart))
      in
      traceShow block $ runGen go (buildEnv bitSize qm) (initialMemory bitSize)

genOperationBlock :: Operation -> Gen (Block Name)
genOperationBlock op = do
  mem <- get

  let (mem', smt) = runOperation mem step op

  put mem'

  bitSize <- envBitSize <$> ask

  pure $ declareMemory bitSize mem' <> one (assert smt)

blockListConstraints :: [Stmt ()] -> Gen (Block Name)
blockListConstraints [] = pure mempty
blockListConstraints (x:xs) = do
  prop <- blockConstraints x
  rest <- mconcat <$> traverse blockConstraints xs
  pure (prop <> rest)

blockConstraints :: Stmt () -> Gen (Block Name)
blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
blockConstraints (SCall f xs) = error "SCall"
blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement

-- TODO: Generalize to applying Hadamard to more than one location
blockConstraints (Partition [lhs] :*=: EHad) = do
  (physStart, physEnd) <- rangeToPhysicalIndices lhs

  genOperationBlock (hadamard physStart)
blockConstraints (SDafny _) = pure mempty

-- TODO: Generalize this
blockConstraints (SIf (GEPartition part Nothing) part' (Qafny.Block [x :*=: ELambda (LambdaF { eBases = [EOp2 OMod (EOp2 OAdd (EVar v) (ENum 1)) (ENum 2)] })])) = do
  let Partition [controlRange] = part
  (physStartControl, physEndControl) <- rangeToPhysicalIndices controlRange

  let Partition [notRange] = x
  (physStartNot, physEndNot) <- rangeToPhysicalIndices notRange

  genOperationBlock (controlledNot physStartControl physStartNot)
blockConstraints s = error $ "unimplemented: " ++ show s

allPossibleBitVectors :: IsString a => Int -> [BitVector a]
allPossibleBitVectors size = map (int2bv size . int) [0 .. 2 ^ size]

applyLambda :: LambdaF (Exp ()) -> Exp () -> Exp ()
applyLambda (LambdaF { bBases = [paramVar], eBases = [body] }) arg =
  subst [(paramVar, arg)] body

type VerifySatisfies =
  Name -> -- Input
  Name -> -- Output
  Gen (Block Name)

data Verify
  = ExactValues [(String, [SMT Name Int])]
  | Satisfies VerifySatisfies

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

