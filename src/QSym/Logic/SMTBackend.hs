-- a small wrapper over SMTLIB.Backends
{-# LANGUAGE OverloadedStrings #-} -- necessary for pretty printing SMT symbols
module QSym.Logic.SMTBackend
  {-(
  -- helper data structures
   SATResult (..)
  ,getFirstResult
  ,isSatisfiable
  ,isUnsatisfiable
  ,isUnknown
  ,didError
  ,didErrorAt
  ,getErrorAt

  ,SMTResult -- you should not need to construct this type
  -- execution functions
  ,executeSMT)-}
	where

-- modules necessary for interacting with QSym.Logic.SMT
import QSym.Logic.SMT (Block)
import Prettyprinter (pretty, Pretty, layoutCompact)
import Prettyprinter.Render.String (renderString)

import Data.String (IsString)

-- modules necessary for interacting with the backend solver
import qualified SMTLIB.Backends as Solver
import qualified SMTLIB.Backends.Process as SMTProcess -- we will be using the process backend, as cvc5 is still experimental

import Control.Monad (forM)

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Builder (stringUtf8)

-- modules necessary for the implementation
import Text.Regex.TDFA

import QSym.Utils (indexed)

import Data.Map (Map)
import qualified Data.Map as Map

{- ## Design:
   What might one want from an API?
    => submit all commands at once
      => swallow output, return results: `executeSMT`
      => stdout output, return results: `executeSMTLoudly`
    => Monadically submit commands and query their result
       This would allow you to check the results of each 
       command and react accordingly (i.e. get-unsat-cores 
       or get-value)

   ### Results from the SMT Solver:
   State after "(check-sat)": sat, unsat, or unknown 
   sat => 
     results of get-value
     results of get-assignment
   unsat => 
     results of get-proof
     results of get-unsat-cores
   potential errors/echo statements
     errors: (error "message")
     echo => "message"
   queries: 
     (1) get-assertions
     (2) get-option
     (3) get-info                                           -}

-----------------------------------------------------
-- Public API
-----------------------------------------------------

-- |SATResult corresponds to the three results that the SMTLIB v2 command "(check-sat)" can return:
--		sat     maps to `Satisfiable`
--		unsat   maps to `Unsatisfiable`
--		unknown maps to `Unknown`
--  and serves as a return type for executeSMT
data SATResult = Satisfiable | Unsatisfiable | Unknown
    deriving (Read, Show, Enum, Eq, Ord)

-- |SMTResult represents the results of executing a block smt statement
-- Information includes:
--   * if any of the commands errored
--   * the results of check-sat, echo, get-value, get-assignment, get-proof, get-unsat-cores, get-assertions, get-option and get-info commands
data SMTResult = SMTResult { errors :: Map Int String -- ^ errors represents the errors returned from executing a block of smt statments as a map of the command's number (starting at 0) mapped to its error (as a string)
                           , satResults :: [SATResult] -- ^ satResults represents a list of all the results of the "check-sat" command, in the order in which it was called. Note: if a check-sat command fails, it will not be included in this list
                           }
    deriving (Read, Show, Eq)

-- |getFirstResult returns the first SATResult in the satResults array of an SMTResult
-- mostly used for executions where only one check-sat command was run
getFirstResult :: SMTResult -> SATResult
getFirstResult smt_result = (satResults smt_result)!!0

-- |isSatisfiable returns true if the first SATResult in the satResults array of an SMTResult is Satisfiable
-- ths use case for this function is if you only have one check-sat command in your SMT block
isSatisfiable :: SMTResult -> Bool
isSatisfiable smt_result = (getFirstResult smt_result) == Satisfiable

-- |isUnsatisfiable returns true if the first SATResult in the satResults array of an SMTResult is Unsatisfiable
-- ths use case for this function is if you only have one check-sat command in your SMT block
isUnsatisfiable :: SMTResult -> Bool
isUnsatisfiable smt_result = (getFirstResult smt_result) == Unsatisfiable

-- |isUnknown returns true if the first SATResult in the satResults array of an SMTResult is Unknown
-- ths use case for this function is if you only have one check-sat command in your SMT block
isUnknown :: SMTResult -> Bool
isUnknown smt_result = (getFirstResult smt_result) == Unknown

-- |didError returns true if an error occured whilst executing the smt commands
didError :: SMTResult -> Bool
didError smt_result = Map.size (errors smt_result) > 0

-- |didErrorAt returns true if an error occured at the specified command number
didErrorAt :: SMTResult -> Int -> Bool
didErrorAt smt_result index = Map.member index (errors smt_result)

-- |getError returns a `Maybe String` value given a command index that an error may have occured at 
getErrorAt :: SMTResult -> Int -> Maybe String
getErrorAt smt_result index = Map.lookup index (errors smt_result)

-- |executeSMT executes a block of SMT statments and returns information about its results. 
-- It will silently swallow any output from the SMT solver process. see its sibling: `executeSMTLoudly` if 
-- you want the output from the SMT solver to be printed to the command line.
-- It takes one argument, of type `Block a` (see: `QSym.Logic.SMT`)
-- It returns a value of type `SMTResult` 
executeSMT :: (IsString a, Pretty a) => Block a -> IO SMTResult
executeSMT smt_code =
  let
    -- split the block statement into its lines (to be used by the solver one at a time)
    statements = lines $ renderString $ layoutCompact $ pretty smt_code
  in
  -- runs a new computation using the 'Process' backend and returns an IO monad
  SMTProcess.with
    cvc5Config
    $ \handle -> do
      -- convert the process handle to an actual backend
      let backend = SMTProcess.toBackend handle
      -- create a solver (w/o queuing because it doesn't work with commands that have no output) from the backend
      solver <- Solver.initSolver Solver.NoQueuing backend
      -- iterate through all the statements and collect their results into one big SMTResult
      fmap fromCommandResults (forM statements (executeSMTStatement solver))

-----------------------------------------------------
-- Private Details
-----------------------------------------------------

-- |cvc5Config returns an `SMTLIB.Backends.Process.Config` that represents the settings for the cvc5 SMT solver. 
cvc5Config :: SMTProcess.Config
cvc5Config = SMTProcess.defaultConfig { SMTProcess.exe = "cvc5", SMTProcess.args = [] } -- defaultConfig for now, consider handling log messages in the future

-- |CommandResult is an internal data type representing the value that a Solver.command can return
data CommandResult 
  = Success 
  | SAT SATResult 
  | Error String
  deriving (Read, Show, Eq)

-- |resultFromString converts from program output to `CommandResult`
resultFromString :: String -> CommandResult
resultFromString str
  | str =~ ("\\`success" :: String) :: Bool = Success
  | str =~ ("\\`sat" :: String) :: Bool     = SAT Satisfiable
  | str =~ ("\\`unsat" :: String) :: Bool   = SAT Unsatisfiable
  | str =~ ("\\`unknown" :: String) :: Bool = SAT Unknown
  | otherwise = let
      errorRegex = "\\(error \"([^\"]+)\"\\)" :: String
      (_, _, _, matches) = str =~ errorRegex :: (String, String, String, [String])
    in
      Error (matches!!0) -- TODO: add checks for no match

-- |returns true if this command result is an error
isError :: CommandResult -> Bool
isError (Error _) = True
isError _ = False

-- |returns true if this command result is a SAT
isSat :: CommandResult -> Bool
isSat (SAT _) = True
isSat _ = False

-- |returns the string from the error message
getErrorMessage :: CommandResult -> String
getErrorMessage (Error msg) = msg
getErrorMessage _ = undefined

-- |returns the `SATResult` from a `SAT`
getSat :: CommandResult -> SATResult
getSat (SAT result) = result
getSat _ = undefined

-- |getErrors returns a map of the integer command number (starting at 0) to the string errors.
getErrors :: [CommandResult] -> Map Int String
getErrors results = Map.fromList $ map (\x -> (fst x, getErrorMessage (snd x))) $ filter (\x -> isError (snd x)) (indexed results)

-- |sats returns a list of the `SATResult`s in an array of `CommandResult`s
sats :: [CommandResult] -> [SATResult]
sats results = map (\x -> getSat x) $ filter (\x -> isSat x) results

-- |fromCommandResults returns an SMTResult from a list of CommandResult
fromCommandResults :: [CommandResult] -> SMTResult
fromCommandResults results = SMTResult (getErrors results) (sats results)

-- |executeSMTStatement executes a single statement (as a `String`) on an SMT solver and returns it's result of type `CommandResult`
executeSMTStatement :: Solver.Solver -> String -> IO CommandResult
executeSMTStatement solver statement = fmap (resultFromString . unpack) (Solver.command solver (stringUtf8 statement))