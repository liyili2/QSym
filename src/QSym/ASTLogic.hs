module QSym.ASTLogic where

import QSym.Syntax
import QSym.QState
import QSym.Monad
import QSym.Utils
import QSym.Logic.Syntax

-- Map of loci and corresponding vars. When a new var is created, it gets added to the front of the corresponding locus's list
type LMap = [(Locus, [Var])]

type Sum = Nat Nat [Complex] [Natural] 

data PredElem = Addr Var | Heap Var | Val QState | V Sum deriving (Eq, Show) -- CLARIFY: Talked briefly last time about maybe changing Addr and Heap to Locus type

-- This is the core type of the predicates respresenting the symbolic executor at each step
data Pred = 
    Equal PredElem PredElem | 
    Less PredElem PredElem |
    Not Pred | 
    And Pred Pred |     
    Or Pred Pred |      
    Imply Pred Pred |
    deriving (Eq, Show, Ord)

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
    Bit Var Nat |   -- Gets boolean value from a bit
    CEq Equal Bit | -- Checks for equality using a control bit | CLARIFY: Not sure if this should be using the Pred Equal or not
    CLess Less Bit  -- Compares values using a control bit
    deriving (Eq, Show, Ord)

data QafnyExpr =
    Apply Locus ArithExpr |     -- Used to apply arithmetic operations on locus in the interperet function
    IfExp BoolExpr QafnyExpr |  -- If BoolExpr, then QanfyExpr. Used in interepret function
    Measure Locus               -- Placeholder for now

-- Finds the current (first index) variable for a given Locus
lookup :: LMap -> Locus -> Var
lookup x:xs loc
    | (fst x) == loc || length xs == 0 = first . lst x -- TODO: Right now it assumes the last element if it gets there, ideally throws an error with an empty list instead
    | otherwise = lookup xs loc

-- Returns the new predicate from applying the QafnyExpr to the old Pred
interpret :: LMap -> Pred -> QafnyExpr -> Pred
interpret lm p (Apply loc expr) = And p (And (Equal (Addr x) (Addr new)) (Equal (Heap new) (Apply expr (Heap x)))) -- CLARIFY: apply expr (Heap x) should be a PredElem, so either PredElem needs new definition or apply needs to be a function
--interpret lm p (IfExp be qe) = CLARIFY: Not really sure what this should like like, just that something else in this file has to be changed
    where x = lookup lm loc
          new = newVar lm loc x -- CLARIFY: Not sure how this works without figuring out how the LMap is tracked first