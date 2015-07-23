{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Arrow (second)
import Control.Applicative hiding (many, optional)
import Control.Exception hiding (try)
import qualified Control.Exception as E
import Control.Monad
import Data.Map hiding (filter, partition)
import qualified Data.Map as M
import Data.List (isPrefixOf)
import Data.Function ()
-- import Data.Text (Text)
import qualified Data.Text as T (justifyLeft, pack, unpack)
-- import qualified Data.Text.IO as TIO
import Data.Typeable
import Prelude hiding (foldl, getContents)
-- import System.Exit (exitFailure, exitSuccess)
import System.Directory
import System.Exit
import System.Environment
import System.Process (rawSystem)
import System.FilePath ((</>))
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec as P ((<|>))
import Text.Parsec.String (Parser)

data Project =
  Project
    {projectName :: String,
     projectIdentifier :: String,
     projectVersion :: (Int, Int, Int)}
  deriving Show

defaultProject :: Project
defaultProject =
  Project
    {projectName = "The Great Indefinix",
     projectIdentifier = "indefinix",
     projectVersion = (0, 0, 0)}

data Wrapper =
  F (String -> Wrapper) |
  A Action
  deriving Show

type Wrapped = (Wrapper, Int)

data CommandError =
  Invalid {invalidInput :: String} |
  Ambiguous {ambiguousInput :: String, candidates :: [String]} |
  Incomplete {incompleteInput :: String, expected :: Int, actual :: Int}
  deriving (Show, Typeable)

instance Exception CommandError

wrap0 :: Action -> Wrapped
wrap0 a = (A a, 0)

wrap1 :: (String -> Action) -> Wrapped
wrap1 f = (F $ \ x -> A $ f x, 1)

wrap2 :: (String -> String -> Action) -> Wrapped
wrap2 f = (F $ \ x -> F $ \ y -> A $ f x y, 2)

commands :: [(String, Wrapped)]
commands =
  [("make", wrap0 Make),
   ("edit", wrap0 Edit),
   ("add", wrap2 Add),
   ("remove", wrap1 Remove),
   ("update", wrap2 Update),
   ("lookup", wrap1 Lookup),
   ("find", wrap1 Find),
   ("touch", wrap0 Touch),
   ("destroy", wrap0 Destroy),
   ("help", wrap0 Help),
   ("version", wrap0 Version)]

parseCommand :: [(String, a)] -> String -> Either CommandError a
parseCommand as x =
  case filter (isPrefixOf x . fst) as of
       ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
       (y : _) -> Right $ snd y
       _ -> Left $ Invalid x

parseActionsWith ::
  Maybe ((String, Int), Wrapped) -> [String] -> Either CommandError [Action]
parseActionsWith (Just ((x, k), (F f, n))) (y : ys) =
  case f y of
       w @ (F _) -> parseActionsWith (Just ((x, k + 1), (w, n))) ys
       A a -> (a :) <$> parseActionsWith Nothing ys
parseActionsWith (Just ((x, k), (F _, n))) _ = Left $ Incomplete x n k
parseActionsWith (Just (_, (A a, _))) ys = (a :) <$> parseActionsWith Nothing ys
parseActionsWith _ (y : ys) =
  case parseCommand commands y of
       Right (w, n) -> parseActionsWith (Just ((y, 0), (w, n))) ys
       Left e -> Left e
parseActionsWith _ _ = Right []

parseActions :: [String] -> Either CommandError [Action]
parseActions = parseActionsWith Nothing

data Action =
  Make |
  Edit |
  Add {addKey :: String, addValue :: String} |
  Remove {removeKey :: String} |
  Update {updateKey :: String, updateValue :: String} |
  Lookup {lookupKey :: String} |
  Find {findKey :: String} |
  Touch |
  Destroy |
  Help |
  Version
  deriving Show

separatorParser :: Parser ()
separatorParser = () <$ char ' ' <* many1 (char ' ')

-- When encountering incorrect indentation:
-- "This does not seem to be a table of contents file. Keep going?"

-- Potential problems:
-- start the file with "key···\n" or "··value\n"

-- Not the best of fixes...
many1Till ::
  (Stream s m t, Show end) =>
  ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end =
  do notFollowedBy end
     first <- p
     rest <- manyTill p end
     return $ first : rest

fileParser :: Parser (Map String String)
fileParser =
  fromList <$> many ((,) <$>
    (many1Till anyChar $ try $ separatorParser) <*>
    (many1Till anyChar $ try $ eof P.<|> () <$
    (newline <* notFollowedBy (() <$ try newline <|> try separatorParser))))

parseContents :: String -> Either ParseError (Map String String)
parseContents = runParser (fileParser <* eof) () []

-- This sucks.
sanitizeContents :: Map String String -> Map String String
sanitizeContents = mapBoth $ filter (/= '\n')

justifyLeft :: Int -> Char -> String -> String
justifyLeft n c = T.unpack . T.justifyLeft n c . T.pack

mapBoth :: Ord b => (a -> b) -> Map a a -> Map b b
mapBoth f = mapKeys f . Data.Map.map f

formatContents :: Map String String -> String
formatContents m =
  let xs = toAscList $ sanitizeContents m
      n = maximum $ length . fst <$> xs in
      unlines $ (\ (x, y) -> justifyLeft n ' ' x ++ "  " ++ y) <$> xs

data ExecutionError =
  AlreadyExists |
  NoEditor |
  EditorFailed {exitCode :: Int} |
  SyntaxError {row :: Int, column :: Int} |
  ContentError {commandError :: ParseError} |
  ConfigurationError {configurationError :: ParseError} |
  SwapInUse |
  Fuck
  deriving (Show, Typeable)

instance Exception ExecutionError

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
executeOne _ Help = putStrLn $ projectName defaultProject
executeOne _ Version = putStrLn $ show $ projectVersion defaultProject

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

data Configuration =
  Configuration
    {name :: String,
     editor :: Maybe String,
     swap :: Maybe (String -> String),
     skip :: Maybe Int,
     wrap :: Maybe Int}
  deriving Show

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    {name = "CONTENTS",
     editor = Nothing,
     swap = Just $ \ x -> "." ++ x ++ ".swap",
     skip = Just 20,
     wrap = Just 80}

execute :: Configuration -> [Action] -> IO ()
execute c = mapM_ $ executeOne c

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

mainWith :: Configuration -> [String] -> IO ()
mainWith c xs =
  case parseActions xs of
       Right x -> execute c x
       Left x -> print x

readConfiguration :: IO Configuration
readConfiguration =
  do fp <- getHomeDirectory
     c <- readFile $ fp </> "." ++ projectIdentifier defaultProject
     case parseConfiguration c of
          Right q -> return q
          Left x -> throw $ ConfigurationError x

parseConfiguration :: String -> Either ParseError Configuration
parseConfiguration = Right . const defaultConfiguration
                           . fromList
                           . fmap (second tail . span (/= '='))
                           . lines

main :: IO ()
main =
  do as <- getArgs
     e <- E.try readConfiguration
     case e of
          Right c -> mainWith c as
          Left (_ :: SomeException) -> mainWith defaultConfiguration as
