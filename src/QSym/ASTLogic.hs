module QSym.ASTLogic
    where

import Prelude hiding (lookup)
import qualified Prelude

import QSym.Syntax
import QSym.QState
import QSym.Monad
import QSym.Utils
import QSym.Logic.Syntax

import Numeric
import Data.Complex

-- Map of loci and corresponding vars. When a new var is created, it gets added to the front of the corresponding locus's list
type LMap = [(Locus, [Var])]

type Sum = Nat Nat [Complex] [Natural]

data PredElem = Addr Var | Heap Var | Val QState | V Sum deriving (Eq, Show)

-- Arithmetic Expression, Comes from page 10 figure 7 in https://arxiv.org/pdf/2211.06411
data ArithExpr = 
    ArithVar Var |              -- Normal Variable
    ArithRange Var Nat Nat |    -- Range of bits in a variable
    Scal Real |                 -- Scalar value
    Add ArithExpr ArithExpr |
    Mult ArithExpr ArithExpr
    deriving (Eq, Show, Ord)

-- Boolean Expression, Comes from page 10 figure 7 in https://arxiv.org/pdf/2211.06411
data BoolExpr =
    Bit Var ArithExpr |   -- Gets boolean value from a bit
    CEq ArithExpr ArithExpr Var ArithExpr | -- Checks for equality using a control bit
    CLess ArithExpr ArithExpr Var ArithExpr  -- Compares values using a control bit
    deriving (Eq, Show, Ord)

-- This is the core type of the predicates respresenting the symbolic executor at each step
data Pred = 
    Equal PredElem PredElem | 
    Less PredElem PredElem |
    Not Pred | 
    And Pred Pred |     
    Or Pred Pred |      
    Imply Pred Pred |
    BEBit BoolExpr
    deriving (Eq, Show, Ord)

data QafnyExpr =
    Apply Locus ArithExpr |     -- Used to apply arithmetic operations on locus in the interperet function
    IfExp BoolExpr QafnyExpr |  -- If BoolExpr, then QanfyExpr. Used in interepret function
    Measure Locus               -- Placeholder for now

-- Finds the current (first index) variable for a given Locus
lookup :: LMap -> Locus -> Maybe Var
lookup [] _ = Nothing
lockup (x:xs) loc = if (fst x == loc) then Just (first . snd x) else lookup xs loc

-- Inserts the new var at the front of the list in the target locus's tuple
-- If the target locus can't be found, creates a new entry tuple at the end of the LMap
addVar :: LMap -> Locus -> Var -> LMap
lookup [] loc v = [(loc, [v])]
lookup (x:xs) loc v = if (fst x == loc) then (loc, v:(snd x)):(lookup xs loc v) else x:(lookup xs loc v)

-- Returns the new predicate from applying the QafnyExpr to the old Pred
interpret :: LMap -> Pred -> QafnyExpr -> (Pred, LMap)
interpret lm p (Apply loc expr) = (And p (And (Equal (Addr x) (Addr new)) (Equal (Heap new) (arithApply expr (Heap x)))), addVar lm new)
interpret lm p IfExp be qe = (Or (And (Not (BEBit be)) p) (And (BEBit be) (interpret lm p qe)), lm)
    where x = lookup lm loc
          new = newVar lm loc x
