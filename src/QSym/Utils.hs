module QSym.Utils
  where

newtype Var = Var Int
  deriving (Show, Eq)

data Posi = Posi Var Int
  deriving (Show, Eq)

newtype RzValue = RzValue (Int -> Bool)

data Value
  = NVal Bool RzValue
  | QVal RzValue RzValue

