module Configuration where

import Control.Arrow (second)
import Control.Exception (throw)
import Data.Map (fromList)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Parsec (ParseError)

import Error

data Constfiguration =
  Constfiguration
    {constName :: String,
     constConfiguration :: String,
     constVersion :: (Int, Int, Int)}
  deriving Show

defaultConstfiguration :: Constfiguration
defaultConstfiguration =
  Constfiguration
    {constName = "The Program Called Contents",
     constConfiguration = ".contents",
     constVersion = (0, 0, 0)}

data Configuration =
  Configuration
    {name :: String,
     editor :: Maybe String,
     swap :: Maybe (String -> String),
     skip :: Maybe Int,
     wrapSkipSlap :: Maybe Int,
     wrap :: Maybe Int}
  -- deriving Show

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    {name = "CONTENTS",
     editor = Nothing,
     swap = Just $ \ x -> "." ++ x ++ ".swap",
     skip = Just 20,
     wrapSkipSlap = Just 2,
     wrap = Just 80}

parseConfiguration :: String -> Either ParseError Configuration
parseConfiguration = Right . const defaultConfiguration
                           . fromList
                           . fmap (second tail . span (/= '='))
                           . lines

readConfiguration :: IO Configuration
readConfiguration =
  do fp <- getHomeDirectory
     c <- readFile $ fp </> constConfiguration defaultConstfiguration
     case parseConfiguration c of
          Right q -> return q
          Left x -> throw $ ConfigurationError x
