{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Test.Tasty

import Test.QuickCheck

import QSym.Monad
import QSym.Interpret
import QSym.Syntax

import Qafny.Syntax.Parser
import Qafny.Syntax.AST
import Qafny.TTG

import Data.Sum

import qualified QSym.Syntax as QSym

-- Bell pair:
--
--
-- q[0] ---> [H] --+------->
--                 |
--                 |
--                 v
-- q[1] --------> (+) ----->


-- TODO: Support for non-unit lists
interpretQafny :: AST -> QSym.Expr
interpretQafny [Toplevel (Inl x)] =
  let Just body = qmBody x
  in
  interpretBlock body

interpretBlock :: Block () -> QSym.Expr
interpretBlock (Block xs) = foldr1 QSym.Seq (map interpretStmt xs)

interpretStmt :: Stmt () -> QSym.Expr
interpretStmt (x :*=: y) = undefined
interpretStmt (SIf cond partition block) = undefined

interpretExpr :: Exp () -> QSym.Expr
interpretExpr EHad = undefined
interpretExpr (EOp2 OAdd x y) = undefined

main :: IO ()
main = do
  (print . scanAndParse) =<< readFile "tests/BellPair.qfy"