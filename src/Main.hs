{-# LANGUAGE CPP, AllowAmbiguousTypes #-}

module Main where

-- required for utf-8 text file enforcement
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.Text as Text

import QSym.Monad
import QSym.Interpret
import QSym.Syntax
import QSym.Logic.GenConstraint (Verify (..), astSMT, Name)
import QSym.Logic.SMTBackend
import QSym.Logic.SMT as SMT --(int, SMT, Decl)

import Qafny.Syntax.Parser
import Qafny.Syntax.AST hiding (Block)
import qualified Qafny.Syntax.AST as AST
import Qafny.TTG

import Data.String

import Data.Sum

import Prettyprinter

import qualified QSym.Syntax as QSym

import Debug.Trace

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

interpretBlock :: AST.Block () -> QSym.Expr
interpretBlock (AST.Block xs) = foldr1 QSym.Seq (map interpretStmt xs)

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
  -- file_text <- Utf8.readFile "tests/Teleportation.qfy"

  -- read in the qafny code and convert into an AST
  -- unpack converts Data.Text.Text (from Utf8.readFile) to a String
  let qafny_ast = (scanAndParse . Text.unpack) file_text 

  print qafny_ast

  -- TODO: potentially add more context to this error message such as filename

  -- let smt = either error (astSMT [[int 1, int 0], [int 1, int 0]] 3) qafny_ast
  -- let smt = either error (astSMT (ExactValues [int 1, int 0, int 1, int 0]) 3) qafny_ast
  let smt = either error (astSMT (Satisfies verify) 4) qafny_ast

  -- either error (print . pretty) smt

  print $ pretty smt
  executeSMTLoudly z3Config smt
  pure ()

verify :: Name -> Name -> Block Name
verify input output =
  smtBlock $
    [assert $ eq (select (symbol input) (int 0)) (int 1)
    ,assert $ eq (select (symbol input) (int 1)) (int 0)
    ,assert $ eq (select (symbol input) (int 2)) (int 1)
    ,assert $ eq (select (symbol input) (int 3)) (int 0)
    ]
    ++
    (go <$> [0,1] <*> [0,1])
  where
    go :: Int -> Int -> SMT Name Decl
    go x y =
      assert $ eq (getOutputValue x y) (expected x y)

    expected :: Int -> Int -> SMT Name Int
    expected x y =
      (getInputValue 0 y
        + (toSign x * getInputValue 1 (invert y)))
        `SMT.div`
      (fromString "sqrt2") -- TODO: Find a better way

    toSign 0 = 1
    toSign 1 = -1
    toSign n = error $ "verify.toSign: " ++ show n

    invert 0 = 1
    invert 1 = 0
    invert n = error $ "verify.invert: " ++ show n

    getInputValue x y =
      select (symbol input) (getIndex x y)

    getOutputValue x y =
      select (symbol output) (getIndex x y)

    getIndex x y =
      int (x + 2^y)

