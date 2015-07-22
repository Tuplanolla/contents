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

data FNAction =
  F0 Action |
  F1 (String -> Action) |
  F2 (String -> String -> Action)
  deriving Show

data Configuration = Configuration
  {name :: String}
  deriving Show

type Contents = Map String String

data PError =
  Invalid String |
  Ambiguous String [String]
  deriving Show

commands :: [(String, FNAction)]
commands =
  [("make", F0 Make),
   ("edit", F0 Edit),
   ("add", F2 Add),
   ("remove", F1 Remove),
   ("update", F2 Update),
   ("lookup", F1 Lookup),
   ("find", F1 Find),
   ("touch", F0 Touch),
   ("destroy", F0 Destroy)]

parseWith :: (FNAction, [Action]) -> String -> Either PError (FNAction, [Action])
parseWith (F2 f, as) x = Right ((F1 $ f x), as)
parseWith (F1 f, as) x = Right ((F0 $ f x), as)
parseWith (F0 a, as) x =
  case filter (\ (y, _) -> x `isPrefixOf` y) commands of
       ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
       ((_, f) : _) -> Right (f, as ++ pure a) -- !!
       _ -> Left $ Invalid x

parseArgs :: [String] -> Either PError [Action]
parseArgs = fmap snd <$> foldM parseWith (F0 None, [])

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
