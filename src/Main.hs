module Main where

import Test.Tasty

import Test.QuickCheck

import QSym.Monad
import QSym.Interpret
import QSym.Syntax

import QSym.Tests.Arithmetic

import Qafny.Syntax.Parser

main :: IO ()
main = do
  (print . scanAndParse) =<< readFile "tests/BellPair.qfy"

  mapM_ quickCheck
    [ checkrzAdder
    -- , checkAdder
    -- , checkDivMod
    ]



