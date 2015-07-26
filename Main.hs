{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Exception
import Data.Map (Map)
import System.Environment
import Text.Parsec (ParseError)

import Command
import Config
import Content
import Error
import Executor
import Project

-- This is avoidable.
#ifdef OS_Linux

import System.Posix

isATTY :: IO Bool
isATTY = queryTerminal stdInput

#else

isATTY :: IO Bool
isATTY = return True

#endif

mainWith :: Config -> [String] -> IO ()
mainWith c xs =
  case parseActions xs of
       Right x -> execute c x
       Left x -> print x

main :: IO ()
main =
  do as <- getArgs
     e <- try readConfig
     b <- isATTY
     case e of
          Right c -> mainWith c as
          Left (_ :: SomeException) -> mainWith defaultConfig as

testc = parseActions ["make", "to", "add", "key", "value", "look", "key"]

testp = parseContents <$> readFile (projectTarget defaultProject)

testf = fmap (formatContents defaultConfig . cleanContents) <$> testp

testq =
  do x <- testf
     case x of
          Right y -> putStr y
          Left e -> print e
