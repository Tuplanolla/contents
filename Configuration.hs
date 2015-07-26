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
     constTarget :: String,
     constSwap :: String,
     constVersion :: (Int, Int, Int)}
  deriving (Eq, Ord, Read, Show)

defaultConstfiguration :: Constfiguration
defaultConstfiguration =
  Constfiguration
    {constName = "The Program Called Contents",
     constConfiguration = ".contents",
     constTarget = "CONTENTS",
     constSwap = ".CONTENTS.swap",
     constVersion = (0, 0, 0)}

data Configuration =
  Configuration
    {editor :: Maybe String,
     skip :: Maybe Int,
     wrap :: Maybe Int}
  deriving (Eq, Ord, Read, Show)

data Policy = MergeAll | KeepFirst | KeepLast | DropAll
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Position = First | Last
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data PlannedConfiguration =
  PlannedConfiguration
     -- The length at which to accept commands, like wa for watch.
    {completion :: Maybe Int,
     -- User editor.
     userEditor :: Maybe String,
     -- Leave this many empty lines between keys.
     lineSkip :: Int,
     -- If a value is too long, cut it before the exceeding word and
     -- move the rest of the value to the next line.
     lineWrap :: Maybe Int,
     -- If a key is too long, move the value to the next line and
     -- do not take the key into account when calculating indents.
     maxKeyLength :: Maybe Int,
     -- The same thing, but taking wrapping into account, so
     -- applies only if (isJust lineWrap).
     minValueLength :: Maybe Int,
     -- If one key is too long, move all keys to the next line and
     -- indent everything based on this.
     fallbackIndent :: Maybe Int,
     -- Order directories somehow.
     positionDirectories :: Maybe Position,
     -- Mark directories with a trailing slash.
     markDirectories :: Bool,
     -- Work with dot-prefixed files too.
     ignoreHidden :: Bool,
     -- Mix existing files not in the contents with the rest when printing.
     showMissing :: Bool,
     -- Hide files in the contents that do not exist.
     hideSpurious :: Bool,
     -- Obvious.
     useSwap :: Bool,
     -- Then rules for special cases!
     -- What to do with duplicate entries.
     duplicatePolicy :: Policy,
     -- Stop when things happen.
     interactiveActions :: Bool,
     interactiveWarnings :: Bool,
     interactiveErrors :: Bool,
     -- Warn about existing inconsistencies.
     warnOrder :: Bool,
     warnDuplicate :: Bool,
     warnMissing :: Bool,
     warnSpurious :: Bool,
     warnLineSkip :: Bool,
     warnIndentation :: Bool}
     -- Perhaps convert Column into (Row, Column) in the parser to accomodate.
  deriving (Eq, Ord, Read, Show)

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    {editor = Nothing,
     skip = Just 20,
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
