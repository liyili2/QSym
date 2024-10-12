module QSym.Logic.ToIR
  where

import QSym.Logic.IR as IR
import QSym.Logic.Gen
import QSym.Logic.Builtins
import Qafny.Syntax.AST as Qafny

blockListConstraints :: [Stmt ()] -> Gen [LoopedSum]
-- blockListConstraints [] = pure mempty
blockListConstraints xs = mconcat <$> traverse blockConstraints xs


blockConstraints :: Stmt () -> Gen [LoopedSum]
blockConstraints (SAssert {}) = pure mempty -- TODO: Should we handle this?
blockConstraints (SCall f xs) = error "SCall"
blockConstraints (SVar {}) = error "SVar: unimplemented" -- TODO: Implement
blockConstraints (_ ::=: EMeasure _) = pure mempty -- TODO: Implement

blockConstraints (_ ::=: _) = error "::=: unimplemented" -- TODO: Implement

blockConstraints (Partition [lhs] :*=: EQft b) = do
  (physStart, physEnd) <- rangeToPhysicalIndices lhs
  pure [NoLoop $ qft b physStart physEnd]

-- TODO: Generalize to applying Hadamard to more than one location
blockConstraints (Partition [lhs] :*=: EHad) = do
  (physStart, physEnd) <- rangeToPhysicalIndices lhs
  pure [NoLoop $ hadamard physStart]

  -- genOperationBlock (hadamard physStart)
blockConstraints (SDafny _) = pure mempty

-- TODO: Generalize this
blockConstraints (SIf guardExp@(GEPartition part Nothing) part' (Qafny.Block body)) = do
  bodyConstraints <- blockListConstraints body

  let Partition [controlRange] = part
  (physStartControl, physEndControl) <- rangeToPhysicalIndices controlRange

  pure $ map (overLooped (withControlBit physStartControl)) bodyConstraints

blockConstraints (x :*=: ELambda (LambdaF { bBases = [param], ePhase = phase, eBases = [lambdaBody :: Exp ()] })) = do
  let Partition [bodyRange] = x
  (physStartBody, physEndBody) <- rangeToPhysicalIndices bodyRange

  let bodySum = convertLambda param physStartBody physEndBody lambdaBody
      updatedPhaseSum = withPhaseFunction (convertPhaseExp param phase) bodySum

  pure [NoLoop bodySum]

blockConstraints (SIf (GClass boolExp) part (Qafny.Block body)) =
    -- TODO: Implement the control here
  blockListConstraints body

blockConstraints (SFor _ _ _ (GEPartition (Partition [range@(Qafny.Range x (ENum start) (ENum end))]) _) _ _ (Qafny.Block body)) = do
  bodyConstraints <- blockListConstraints body
  pure [ForIn x range bodyConstraints]

blockConstraints s = error $ "unimplemented: " ++ show s

-- TODO
convertPhaseExp :: Var -> PhaseExp -> (Expr Int -> Expr Int)
convertPhaseExp param PhaseWildCard e = e
convertPhaseExp param PhaseZ e = e
convertPhaseExp param (PhaseOmega x y) e = e

convertLambda :: Var -> Int -> Int -> Exp () -> Sum
convertLambda param startQubit endQubit body =
  unaryIntOp (convertLambdaBody param body) startQubit endQubit

convertLambdaBody :: Var -> Exp () -> (Expr Int -> Expr Int)
convertLambdaBody param (ENum i) _ = intLit i
convertLambdaBody param (EVar v) arg
  | v == param = arg
  | otherwise = error $ "convertLambdaBody: EVar " ++ show v
convertLambdaBody param (EOp1 op x) arg = convertOp1 param op x arg
convertLambdaBody param (EOp2 op x y) arg = convertOp2 param op x y arg

convertOp1 :: Var -> Op1 -> Exp () -> (Expr Int -> Expr Int)
convertOp1 param ONeg x arg = neg (convertLambdaBody param x arg)

convertOp2 :: Var -> Op2 -> Exp () -> Exp () -> (Expr Int -> Expr Int)
convertOp2 param op x y arg =
  case op of
    OAdd -> go IR.add
    OSub -> go IR.sub
    OMul -> go IR.mul
    OMod -> go IR.modulo
  where
    go f = f (convertLambdaBody param x arg) (convertLambdaBody param y arg)

