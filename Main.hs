{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative as A
import Control.Exception
import Control.Monad
import Data.Map hiding (filter, partition)
import Data.List hiding (insert)
import Data.Typeable
import Prelude hiding (foldl, getContents)
-- import System.Exit (exitFailure, exitSuccess)
import System.IO hiding (getContents)
import System.IO.Error
import System.Environment
import System.Process (rawSystem)
-- import Text.Parsec
-- import Text.Parsec.Text (Parser)

type Contents = Map String String

data Wrapper =
  F (String -> Wrapper) |
  A Action
  deriving Show

type Wrapped = (Wrapper, Int)

data ParsingError =
  Invalid {invalidInput :: String} |
  Ambiguous {ambiguousInput :: String, candidates :: [String]} |
  Incomplete {incompleteInput :: String, expected :: Int, actual :: Int}
  deriving (Show, Typeable)

instance Exception ParsingError

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

parseCommand :: [(String, a)] -> String -> Either ParsingError a
parseCommand as x =
  case filter (isPrefixOf x . fst) as of
       ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
       (y : _) -> Right $ snd y
       _ -> Left $ Invalid x

parseActionsWith :: Maybe ((String, Int), Wrapped) -> [String] -> Either ParsingError [Action]
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

parseActions :: [String] -> Either ParsingError [Action]
parseActions = parseActionsWith Nothing

fromString :: String -> Contents
fromString = fromList . fmap (partition (== ' ')) . lines
-- Parsec here.

toString :: Contents -> String
toString = unlines . fmap (\ (x, y) -> x ++ "  " ++ y) . toAscList

data ExecutionError =
  AlreadyExists |
  NoEditor
  deriving (Show, Typeable)

instance Exception ExecutionError

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

executeOne :: Configuration -> Action -> IO ()
executeOne c Make =
  catchJust -- This contains a possible race condition.
    (\ e -> if isDoesNotExistError e then Just () else Nothing)
    (do openFile (name c) ReadMode >>= hClose
        throw AlreadyExists)
    (\ _ -> openFile (name c) WriteMode >>= hClose)
executeOne c Edit =
  do m <- (<|>) <$> return (editor c) <*> lookupEnv "EDITOR"
     case m of
          Just x -> do _ <- rawSystem x [name c]
                       return ()
          _ -> throw NoEditor
executeOne c (Add k v) =
  do x <- readFile $ name c
     let y = toString $ insert k v $ fromString x
     _ <- evaluate $ length x
     writeFile (name c) y
     -- Write into swap file instead.

data Configuration =
  Configuration {name :: String, editor :: Maybe String, swap :: String -> String}
  deriving Show

execute :: Configuration -> [Action] -> IO ()
execute c = mapM_ $ executeOne c

test :: Either ParsingError [Action]
test = parseActions ["make", "to", "add", "key", "value", "look", "key"]

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration {name = "CONTENTS", editor = Just "vim", swap = \ x -> "." ++ x ++ ".swap"}

mainWith :: [String] -> IO ()
mainWith xs =
  case parseActions xs of
       Right x -> execute defaultConfiguration x
       Left x -> print x

main :: IO ()
main =
  do xs <- getArgs
     mainWith xs
