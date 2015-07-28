module Executor where

import Control.Applicative (Alternative((<|>)), Applicative((<*>)), (<$>))
import Control.Exception (evaluate, throw)
import Data.Map (Map, delete, insert)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure))
import System.Process (rawSystem)
import qualified Data.Map as M (lookup)

import Config
import Table
import Command
import Error
import Extra
import Project

executeOne :: Config -> Action -> IO ()
executeOne _ Make =
  do let file = projectTarget defaultProject
     b <- doesFileExist $ projectTarget defaultProject
     if b then
        throw AlreadyExists else
        -- If another process creates 'file' here,
        -- it will be overwritten by this process.
        writeFile file ""
executeOne Config {editor = editor} Edit =
  do let file = projectTarget defaultProject
     m <- getEditor
     case editor <|> m of
          Just x ->
            do e <- rawSystem x [file]
               case e of
                    ExitFailure n -> throw $ EditorFailed x n
                    _ -> return ()
          _ -> throw NoEditor
-- These write the file even when they should not.
-- They also need additional access to the formatted result.
executeOne c (Add k v) = changeTables c $ return . insert k v -- Ensure does not exist.
executeOne c (Remove k) = changeTables c $ return . delete k
executeOne c (Update k v) = changeTables c $ return . insert k v -- Ensure exists.
executeOne c (Lookup k) = changeTables c $ \ m -> print (M.lookup k m) >> return m -- No.
executeOne c (Find k) = changeTables c $ \ m -> print (M.lookup k m) >> return m -- No.
executeOne c Touch = changeTables c $ return
executeOne _ Destroy = removeFile $ projectTarget defaultProject
executeOne _ Help = putStrLn $ projectName defaultProject
executeOne _ Version = putStrLn $ show $ projectVersion defaultProject

execute :: Config -> [Action] -> IO ()
execute c = mapM_ $ executeOne c

-- Use these in the future.
-- getHomeDirectory :: IO FilePath
-- doesFileExist :: FilePath -> IO Bool

changeTables ::
  Config -> (Map String String -> IO (Map String String)) -> IO ()
changeTables c f =
  do let file = projectTarget defaultProject
         swapFile = projectSwap defaultProject
     x <- readFile file
     _ <- evaluate $ length x
     case cleanTables <$> parseTables x of
          Right y ->
            do fp <- getCurrentDirectory
               fps <- getDirectoryContents fp
               _ <- return fps
               -- Here goes something to merge reality with expectations.
               -- [FilePath] -> Map String String -> Map String (Maybe String, Bool)
               -- That Maybe says whether there is an entry and
               -- that Bool says whether a file exists.
               q <- f y
               let z = formatTables c q
               if True then
                  do b <- doesFileExist swapFile
                     -- If another process creates swapFile here,
                     -- it will be overwritten by this process.
                     if b then
                        throw SwapInUse else
                        do writeFile swapFile z
                           -- This only works if swapFile
                           -- is on the same volume as file.
                           renameFile swapFile file else
                  writeFile file z
          Left e -> throw e

readTables :: Config -> IO (Map String String)
readTables _ =
  do let file = projectTarget defaultProject
     x <- readFile file
     _ <- evaluate $ length x
     case cleanTables <$> parseTables x of
          Right y -> return y
          Left e -> throw e
