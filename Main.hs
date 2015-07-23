{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>))
import Control.Exception (SomeException)
import Data.Map (Map)
import qualified Control.Exception as E (try)
import System.Environment (getArgs)
import Text.Parsec (ParseError)

import Command
import Configuration
import Content
import Error
import Executor

mainWith :: Configuration -> [String] -> IO ()
mainWith c xs =
  case parseActions xs of
       Right x -> execute c x
       Left x -> print x

main :: IO ()
main =
  do as <- getArgs
     e <- E.try readConfiguration
     case e of
          Right c -> mainWith c as
          Left (_ :: SomeException) -> mainWith defaultConfiguration as

testc :: Either CommandError [Action]
testc = parseActions ["make", "to", "add", "key", "value", "look", "key"]

testp :: IO (Either ParseError (Map String String))
testp = parseContents <$> readFile (name defaultConfiguration)

testf :: IO (Either ParseError String)
testf = fmap formatContents <$> testp

testq :: IO ()
testq =
  do x <- testf
     case x of
          Right y -> putStr y
          Left e -> print e
