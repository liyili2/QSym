{-# LANGUAGE CPP, AllowAmbiguousTypes #-}

module Main where

-- required for utf-8 text file enforcement
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.Text as Text

import QSym.Monad
import QSym.Interpret
import QSym.Syntax
import QSym.Logic.GenConstraint (astConstraints)
import QSym.Logic.SMTBackend

import Qafny.Syntax.Parser
import Qafny.Syntax.AST
import Qafny.TTG

import Data.Sum

import Prettyprinter

import qualified QSym.Syntax as QSym

-- Bell pair example:
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
  -- TODO: read filename in from the command line
  file_text <- Utf8.readFile "tests/BellPair.qfy"
  -- read in the qafny code and convert into an AST
  -- unpack converts Data.Text.Text (from Utf8.readFile) to a String
  let qafny_ast = (scanAndParse . Text.unpack) file_text 

  print qafny_ast

  -- TODO: potentially add more context to this error message such as filename
  let smt = either error (astConstraints 3) qafny_ast
  -- either error (print . pretty) smt

  print $ pretty smt
  executeSMTLoudly smt
  pure ()

