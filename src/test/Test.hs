module Main where

import System.Exit -- needed for exitSuccess and exitFailure

import Test.Tasty

import Test.QuickCheck

import QSym.Monad
import QSym.Interpret
import QSym.Syntax

import QSym.Tests.Arithmetic

main :: IO ()
main = do
{
  -- run all of the tests and create a list of the results of each
  result <- mapM test
    [ Named "initV" checkInitV,
      Named "flipBits" checkFlipBits,
      Named "notIsHighBitSet" checkNotIsHighBitSet,
      Named "comparator" checkComparator
    --checkrzDivMod
    --checkrzSub
    -- , checkAdder
    -- , checkrzAdder
    ];
  -- ensure all of the results are a success
  case all isSuccess result of
    True -> exitSuccess
    False -> exitFailure
}

--------------------------------------
-- Private details
--------------------------------------

-- |TestCase represents a singular test case and allows attaching 
-- names to the cases (useful for logs)
data TestCase = Named String Property | Unnamed Property

-- runs the actual test case and prints it's name (if it has one)
test :: TestCase -> IO Result
test (Named name f) = do { putStrLn("Running " ++ name ++ " test..."); quickCheckResult f }
test (Unnamed f) = quickCheckResult f