module QSym.Utils
  where

newtype Var = Var Int
  deriving (Show, Eq)

data Posi = Posi Var Int
  deriving (Show, Eq)

newtype RzValue = RzValue (Int -> Bool)

(!) :: RzValue -> Int -> Bool
RzValue f ! i = f i

-- indexRzValue :: RzValue -> Int -> Bool
-- indexRzValue (RzValue f) = f

data Value
  = NVal Bool RzValue
  | QVal RzValue RzValue

