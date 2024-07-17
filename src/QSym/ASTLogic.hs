
import QSym.Syntax
import QSym.QState
import QSym.Monad
import QSym.Utils
import QSym.Logic.Syntax
import QSym.Tests.Arithmetic

--Is a normal Var with the addition of an address field to keep track of the Locus it represents
data PredVar = {value :: Var, address :: Locus}

type Sum = Nat Nat [Complex] [Natural]

data PredElem = Addr Var | Heap Var | Val QState | V Sum deriving (Eq, Show)

--This is the core type of the predicates respresenting the symbolic executor at each step
data Pred = 
    Equal PredElem PredElem |
    Less PredElem PredElem |
    Not Pred | 
    And Pred Pred |     
    Or Pred Pred |      
    Imply Pred Pred
    deriving (Eq, Show, Ord)

-- Arithmetic Expression, Comes from page 10 figure 7 in https://arxiv.org/pdf/2211.06411
data ArithExpr = 

-- Boolean Expression, Comes from page 10 figure 7 in https://arxiv.org/pdf/2211.06411
data BoolExpr =

data QafnyExpr =
    Apply Locus ArithExpr | -- Used to apply arithmetic operations on locus in the interperet function
    IfExp BoolExpr QafnyExpr | -- If BoolExpr, then QanfyExpr. Used in interepret function
    Measure Locus -- Placeholder for now

interpret :: LMap -> Pred -> QafnyExpr -> Pred
interpret lm p (Apply loc expr) = And p (And (Equal (Addr x)) (Addr new)) (Equal (Heap new) (Apply e (Heap x)))
    where x = lookup lm loc 



--TODO: LMap should be able to link vars to loci and keep track of new vars
--Implement lookup for LMap