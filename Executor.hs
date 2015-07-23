{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

module Executor where

import Control.Applicative (Alternative((<|>)), Applicative((<*>)), (<$>))
import Control.Exception (evaluate, throw)
import Data.Map (Map, delete, insert)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure))
import System.Process (rawSystem)
import qualified Data.Map as M (lookup)

import Configuration
import Content
import Command
import Error

executeOne :: Configuration -> Action -> IO ()
executeOne Configuration {name = file} Make =
  do b <- doesFileExist file
     -- If another process creates file here,
     -- it will be overwritten by this process.
     if b then
        throw AlreadyExists else
        writeFile file ""
executeOne Configuration {name = file, editor = program} Edit =
  do m <- (<|>) <$> return program <*> lookupEnv "EDITOR" -- Move elsewhere.
     case m of
          Just x ->
            do e <- rawSystem x [file]
               case e of
                    ExitFailure n -> throw $ EditorFailed n
                    _ -> return ()
          _ -> throw NoEditor
-- These write the file even when they should not.
-- They also need additional access to the formatted result.
executeOne c (Add k v) = changeContents c $ return . insert k v -- Ensure does not exist.
executeOne c (Remove k) = changeContents c $ return . delete k
executeOne c (Update k v) = changeContents c $ return . insert k v -- Ensure exists.
executeOne c (Lookup k) = changeContents c $ \ m -> print (M.lookup k m) >> return m -- No.
executeOne c (Find k) = changeContents c $ \ m -> print (M.lookup k m) >> return m -- No.
executeOne c Touch = changeContents c $ return
executeOne Configuration {name = file} Destroy = removeFile file
executeOne _ Help = putStrLn $ constName defaultConstfiguration
executeOne _ Version = putStrLn $ show $ constVersion defaultConstfiguration

execute :: Configuration -> [Action] -> IO ()
execute c = mapM_ $ executeOne c

-- Use these in the future.
-- getHomeDirectory :: IO FilePath
-- doesFileExist :: FilePath -> IO Bool

changeContents ::
  Configuration -> (Map String String -> IO (Map String String)) -> IO ()
changeContents c @ Configuration {name = file} f =
  do x <- readFile file
     _ <- evaluate $ length x
     case parseContents x of
          Right y ->
            do fp <- getCurrentDirectory
               fps <- getDirectoryContents fp
               _ <- return fps
               -- Here goes something to merge reality with expectations.
               -- [FilePath] -> Map String String -> Map String (Maybe String, Bool)
               -- That Maybe says whether there is an entry and
               -- that Bool says whether a file exists.
               q <- f y
               let z = formatContents q
               case swap c of
                    Just g ->
                      do let swapFile = g file
                         b <- doesFileExist swapFile
                         -- If another process creates swapFile here,
                         -- it will be overwritten by this process.
                         if b then
                            throw SwapInUse else
                            do writeFile swapFile z
                               -- This only works if swapFile
                               -- is on the same volume as file.
                               renameFile swapFile file
                    _ -> writeFile file z
          Left e -> throw $ ContentError e
