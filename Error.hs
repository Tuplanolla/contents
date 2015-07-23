{-# LANGUAGE DeriveDataTypeable #-}

module Error where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Text.Parsec (ParseError)

data CommandError =
  Invalid {invalidInput :: String} |
  Ambiguous {ambiguousInput :: String, candidates :: [String]} |
  Incomplete {incompleteInput :: String, expected :: Int, actual :: Int}
  deriving (Show, Typeable)

instance Exception CommandError

data ExecutionError =
  AlreadyExists |
  NoEditor |
  EditorFailed {editorName :: String, exitCode :: Int} |
  SyntaxError {row :: Int, column :: Int} |
  ContentError {commandError :: ParseError} |
  ConfigurationError {configurationError :: ParseError} |
  SwapInUse |
  Fuck
  deriving (Show, Typeable)

instance Exception ExecutionError
