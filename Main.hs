{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char
import Data.Map hiding (filter, partition)
import Data.List hiding (insert)
import Prelude hiding (foldl, getContents)
import System.Exit (exitFailure, exitSuccess)
import System.IO hiding (getContents)
import System.IO.Error
import System.Environment
import Text.Parsec
import Text.Parsec.Text (Parser)

data Action =
  None |
  Make |
  Edit |
  Add String String |
  Remove String |
  Update String String |
  Lookup String |
  Find String |
  Touch |
  Destroy
  deriving Show

data Configuration = Configuration
  {name :: String}
  deriving Show

type Contents = Map String String

data Wrapper =
  F (String -> Wrapper) |
  A Action
  deriving Show

data CommandError =
  Invalid {input :: String} |
  Ambiguous {input :: String, candidates :: [String]} |
  Incomplete {input :: String, expected :: Int, actual :: Int}
  deriving Show

liftN0 a = A a
liftN1 f = F $ \ x -> A $ f x
liftN2 f = F $ \ x -> F $ \ y -> A $ f x y

commands :: [(String, Wrapper)]
commands =
  [("make", liftN0 Make),
   ("edit", liftN0 Edit),
   ("add", liftN2 Add),
   ("remove", liftN1 Remove),
   ("update", liftN2 Update),
   ("lookup", liftN1 Lookup),
   ("find", liftN1 Find),
   ("touch", liftN0 Touch),
   ("destroy", liftN0 Destroy)]

readCommand :: [(String, a)] -> String -> Either CommandError a
readCommand as x =
  case filter (isPrefixOf x . fst) as of
       ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
       (y : _) -> Right $ snd y
       _ -> Left $ Invalid x

parseWith :: Maybe (String, Wrapper) -> [String] -> Either CommandError [Action]
parseWith (Just (x, F f)) (y : ys) =
  case f y of
       w @ (F _) -> parseWith (Just (x, w)) ys
       A a -> (a :) <$> parseWith Nothing ys
parseWith (Just (x, F _)) _ = Left $ Incomplete x 0 0
parseWith (Just (x, A a)) ys = (a :) <$> parseWith Nothing ys
parseWith _ (y : ys) =
  case readCommand commands y of
       Right w -> parseWith (Just (y, w)) ys
       Left e -> Left e
parseWith _ _ = Right []

parseArgs :: [String] -> Either CommandError [Action]
parseArgs = parseWith Nothing

fromString :: String -> Contents
fromString = fromList . fmap (partition (== ' ')) . lines
-- Parsec here.

toString :: Contents -> String
toString = unlines . fmap (\ (x, y) -> x ++ "  " ++ y) . toAscList

executeOne :: Configuration -> Action -> IO ()
executeOne c Make =
  catchJust (\ e -> if isDoesNotExistError e then Just () else Nothing)
            (do openFile (name c) ReadMode >>= hClose
                fail "file existed just a moment ago") -- Possible race condition.
            (\ _ -> openFile (name c) WriteMode >>= hClose)
            -- Open with AppendMode instead and check cursor.
executeOne c Edit = undefined
executeOne c (Add k v) =
  do x <- readFile (name c)
     let y = toString $ insert k v $ fromString x
     evaluate $ length x
     writeFile (name c) y
     -- Write into swap file instead.

execute :: Configuration -> [Action] -> IO ()
execute c = mapM_ $ executeOne c

test = parseArgs ["make", "to", "add", "key", "value", "look", "key"]

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration {name = "CONTENTS"}

main :: IO ()
main =
  do xs <- getArgs
     case parseArgs xs of
          Right x -> execute defaultConfiguration x
          Left x -> print x
