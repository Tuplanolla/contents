{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Exception
import Data.Map (Map)
import System.Environment
import System.IO.Error
import Text.Parsec (ParseError)

import Command
import Config
import Table
import Error
import Executor
import Platform
import Project

mainWith :: Project -> Config -> [String] -> IO ()
mainWith p c xs =
  case parseActions xs of
       Right x -> execute p c x
       Left x -> print x

main :: IO ()
main =
  do as <- getArgs
     e <-
       tryJust (\ e ->
       -- isAlreadyInUseError
       -- isDoesNotExistError
       -- isPermissionError
       if isDoesNotExistError e then Just 1 else Nothing)
       readConfig
     b <- isInputInteractive
     -- Needs a better exception mechanism.
     mainWith defaultProject (
       case e of
            Right c -> c
            Left 1 -> defaultConfig
       ) {interactive = b} as

testc = parseActions ["make", "to", "add", "key", "value", "look", "key"]

testp = parseTable <$> readFile (projectTarget defaultProject)

testf = fmap (formatTable defaultConfig . cleanTable . stripPositions) <$> testp

testq =
  do x <- testf
     case x of
          Right y -> putStr y
          Left e -> print e
