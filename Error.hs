{-# LANGUAGE DeriveDataTypeable #-}

module Error where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data ExecutionError =
  Invalid {invalidInput :: String} |
  Ambiguous {ambiguousInput :: String, candidates :: [String]} |
  Incomplete {incompleteInput :: String, expectedInput :: Int, actualInput :: Int} |
  AlreadyExists |
  NoEditor |
  EditorFailed {editorName :: String, exitCode :: Int} |
  SyntaxError {row :: Int, column :: Int} |
  SwapInUse |
  AlreadyInThere String |
  NotInThere String |
  NotFound String |
  Fuck
  deriving (Eq, Ord, Read, Show, Typeable)

instance Exception ExecutionError
