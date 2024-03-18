module Main where

import QSym.Tests.Arithmetic
import Test.QuickCheck

main :: IO ()
main =
  quickCheck checkDivMod
