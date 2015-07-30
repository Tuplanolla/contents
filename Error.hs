{-# LANGUAGE DeriveDataTypeable #-}

module Error where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data ContentsError =
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
  NotFound String
  deriving (Eq, Ord, Read, Show, Typeable)

instance Exception ContentsError

data ExecutionWarning =
  WLineSkip |
  WDuplicate |
  WOrder |
  WIndentation
  deriving (Eq, Ord, Read, Show)
