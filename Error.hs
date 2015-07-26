{-# LANGUAGE DeriveDataTypeable #-}

module Error where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Text.Parsec (ParseError)

data CommandError =
  Invalid {invalidInput :: String} |
  Ambiguous {ambiguousInput :: String, candidates :: [String]} |
  Incomplete {incompleteInput :: String, expected :: Int, actual :: Int}
  deriving (Eq, Ord, Read, Show, Typeable)

instance Exception CommandError

data ExecutionError =
  AlreadyExists |
  NoEditor |
  EditorFailed {editorName :: String, exitCode :: Int} |
  SyntaxError {row :: Int, column :: Int, expected :: String, actual :: String} |
  -- Should not stuff Parsec's errors here.
  ContentError {contentError :: ParseError} |
  ConfigurationError {configurationError :: ParseError} |
  SwapInUse |
  Fuck
  deriving (Show, Typeable)
  -- deriving (Eq, Ord, Read, Show, Typeable)

instance Exception ExecutionError
