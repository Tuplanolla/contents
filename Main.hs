{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative hiding (many, optional)
import Control.Exception hiding (try)
import Control.Monad
import Data.Map hiding (filter, partition)
import Data.List (isPrefixOf, partition)
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import Data.Typeable
import Prelude hiding (foldl, getContents)
-- import System.Exit (exitFailure, exitSuccess)
import System.IO hiding (getContents)
import System.IO.Error
import System.Exit
import System.Environment
import System.Process (rawSystem)
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec as P ((<|>))
import Text.Parsec.String (Parser)

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

parseActionsWith :: Maybe ((String, Int), Wrapped) -> [String] -> Either CommandError [Action]
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

fromString :: String -> Map String String
fromString = fromList . fmap (partition (== ' ')) . lines
-- Parsec here.

toString :: Map String String -> String
toString = unlines . fmap (\ (x, y) -> x ++ "  " ++ y) . toAscList

data ExecutionError =
  AlreadyExists |
  NoEditor |
  EditorFailed Int |
  Fuck
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

separatorParser :: Parser ()
separatorParser = () <$ (space <* many1 space)

continuationParser :: Parser ()
continuationParser = () <$ many1 space

fileParser :: Parser (Map String String)
fileParser =
  fromList <$> many ((,) <$>
    (manyTill anyChar $ try $ separatorParser) <*>
    (manyTill anyChar $ try $ eof P.<|> () <$ (newline <* notFollowedBy (try continuationParser))))

parseContents :: String -> Either ParseError (Map String String)
parseContents = runParser fileParser () []

executeOne :: Configuration -> Action -> IO ()
executeOne c Make =
  catchJust -- This contains a possible race condition.
    (\ e -> if isDoesNotExistError e then Just () else Nothing)
    (do openFile (name c) ReadMode >>= hClose
        throw AlreadyExists)
    (\ _ -> openFile (name c) WriteMode >>= hClose)
executeOne c Edit =
  do m <- (<|>) <$> return (editor c) <*> lookupEnv "EDITOR" -- Check this.
     case m of
          Just x -> do e <- rawSystem x [name c]
                       case e of
                            ExitFailure n -> throw $ EditorFailed n
                            _ -> return ()
          _ -> throw NoEditor
executeOne c (Add k v) =
  do x <- readFile $ name c
     _ <- evaluate $ length x
     let y = toString $ insert k v $ fromString x
     writeFile (name c) y
     -- Write into swap file instead.
executeOne c (Remove k) =
  do x <- readFile $ name c
     _ <- evaluate $ length x
     let y = toString $ delete k $ fromString x
     writeFile (name c) y
-- Not implemented yet.
executeOne c (Update k v) = (c, k, v) `seq` undefined
executeOne c (Lookup k) = (c, k) `seq` undefined
executeOne c (Find k) = (c, k) `seq` undefined
executeOne c Touch = c `seq` undefined
executeOne c Destroy = c `seq` undefined
executeOne c Help = c `seq` undefined
executeOne c Version = c `seq` undefined

data Configuration =
  Configuration {name :: String, editor :: Maybe String, swap :: String -> String}
  deriving Show

execute :: Configuration -> [Action] -> IO ()
execute c = mapM_ $ executeOne c

test :: Either CommandError [Action]
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
