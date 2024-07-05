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
  result <- mapM quickCheckResult
    [  checkInitV,
       checkFlipBits,
       checkNotIsHighBitSet,
       checkAddAndCompare
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
