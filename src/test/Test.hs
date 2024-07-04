module Main where

import Test.Tasty

import Test.QuickCheck

import QSym.Monad
import QSym.Interpret
import QSym.Syntax

import QSym.Tests.Arithmetic

main :: IO ()
main = 
  mapM_ quickCheck
    [  checkInitV,
       checkFlipBits,
       checkNotIsHighBitSet
    --checkrzDivMod
      --checkrzSub
    -- , checkAdder
    -- , checkrzAdder
    ]
