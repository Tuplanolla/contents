module Executor where

import Control.Applicative (Alternative((<|>)), Applicative((<*>)), (<$>))
import Control.Exception (evaluate, throw)
import Data.List (isInfixOf)
import Data.Map
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

executeOne :: Project -> Config -> Action -> IO ()
executeOne p _ Create =
  do let file = projectTarget p
     b <- doesFileExist $ projectTarget p
     if b then
        throw AlreadyExists else
        -- If another process creates 'file' here,
        -- it will be overwritten by this process.
        createFile file
executeOne p Config {editor = editor} Edit =
  do let file = projectTarget p
     m <- getEditor
     case editor <|> m of
          Just x ->
            do e <- rawSystem x [file]
               case e of
                    ExitFailure n -> throw $ EditorFailed x n
                    _ -> return () -- Sanitize here.
          _ -> throw NoEditor
-- These write the file even when they should not.
-- They also need additional access to the formatted result.
executeOne p c (Add k v) =
  changeTable p c $ \ m ->
    if member k m then
       throw $ AlreadyInThere k else
       return $ insert k v m
executeOne p c (Remove k) =
  changeTable p c $ \ m ->
    if notMember k m then
       throw $ NotInThere k else
       return $ delete k m
executeOne p c (Update k v) =
  changeTable p c $ \ m ->
    if notMember k m then
       throw $ NotInThere k else
       return $ insert k v m
executeOne p c (Lookup k) =
  withTable p c $ \ m ->
    case M.lookup k m of
         Just v -> putStrLn v
         Nothing -> throw $ NotInThere k
executeOne p c (Find x) =
  withTable p c $ \ m ->
    -- This should form another table and format it, but
    -- the basic logic is there.
    case toAscList $ filterWithKey (\ k v -> x `isInfixOf` k || x `isInfixOf` v) m of
         xs @ (_ : _) -> putStr $ unlines $ (\ (k, v) -> k ++ "  " ++ v) <$> xs
         _ -> throw $ NotFound x
-- Interact
-- FindKey
-- FindValue
executeOne p c Touch = changeTable p c $ return
-- Catch exceptions in here.
executeOne p _ Destroy = removeFile $ projectTarget p
executeOne p _ Help = putStrLn $ projectName p
executeOne p _ Version = putStrLn $ show $ projectVersion p

execute :: Project -> Config -> [Action] -> IO ()
execute p c = mapM_ $ executeOne p c

changeTable ::
  Project -> Config -> (Map String String -> IO (Map String String)) -> IO ()
changeTable p c f =
  do let file = projectTarget p
         swapFile = projectSwap p
     x <- readFile file
     _ <- evaluate $ length x
     case cleanTable <$> parseTable x of
          Right y ->
            do fp <- getCurrentDirectory
               fps <- getDirectoryContents fp
               _ <- return fps
               -- Here goes something to merge reality with expectations.
               -- [FilePath] -> Map String String -> Map String (Maybe String, Bool)
               -- That Maybe says whether there is an entry and
               -- that Bool says whether a file exists.
               q <- f y
               let z = formatTable c q
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

withTable :: Project -> Config -> (Map String String -> IO ()) -> IO ()
withTable p _ f =
  do let file = projectTarget p
         swapFile = projectSwap p
     x <- readFile file
     _ <- evaluate $ length x
     case cleanTable <$> parseTable x of
          Right y ->
            do fp <- getCurrentDirectory
               fps <- getDirectoryContents fp
               _ <- return fps
               -- Only the next line differs.
               f y
          Left e -> throw e
