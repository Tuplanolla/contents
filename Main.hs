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

data Nested =
  O |
  I Action |
  N (String -> Nested)
  deriving Show

data Configuration = Configuration
  {name :: String}
  deriving Show

type Contents = Map String String

data PError =
  Invalid String |
  Ambiguous String [String]
  deriving Show

liftN0 a = I a
liftN1 f = N $ \ x -> I $ f x
liftN2 f = N $ \ x -> N $ \ y -> I $ Add x y

commands :: [(String, Nested)]
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

parseWith :: (Nested, [Action]) -> String -> Either PError (Nested, [Action])
parseWith (N f, as) x = Right (f x, as)
parseWith (I a, as) x = Right (O, a : as)
parseWith (O, as) x =
  case filter (\ (y, _) -> x `isPrefixOf` y) commands of
       ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
       ((_, f) : _) -> Right (f, as)
       _ -> Left $ Invalid x

parseArgs :: [String] -> Either PError [Action]
parseArgs = fmap snd <$> foldM parseWith (O, [])

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
