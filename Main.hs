{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Exception
import Data.Map (Map)
import System.Environment
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
     e <- try readConfiguration
     case e of
          Right c -> mainWith c as
          Left (_ :: SomeException) -> mainWith defaultConfiguration as

testc = parseActions ["make", "to", "add", "key", "value", "look", "key"]

testp = parseContents <$> readFile (constTarget defaultConstfiguration)

testf = fmap (formatContents defaultConfiguration . cleanContents) <$> testp

testq =
  do x <- testf
     case x of
          Right y -> putStr y
          Left e -> print e
