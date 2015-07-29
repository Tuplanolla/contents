module Config where

import Control.Arrow (left)
import Control.Exception (throw)
import Data.Map (fromList)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Parsec (ParseError)
import Text.Read (readEither)

import Error
import Project

data Policy = MergeAll | KeepFirst | KeepLast | DropAll
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Position = First | Last
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- Consider using a Map too.
data Config =
  Config
     -- The length at which to accept commands, like wa for watch.
    {completion :: Maybe Int,
     -- User's favorite editor.
     editor :: Maybe String,
     -- Leave this many empty lines between keys.
     skip :: Int,
     -- If a value is too long, cut it before the exceeding word and
     -- move the rest of the value to the next line.
     wrap :: Maybe Int,
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
     -- What to show for entries that do not have values.
     placeholder :: String,
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
     warnMarker :: Bool,
     warnMissing :: Bool,
     warnSpurious :: Bool,
     -- These are pure.
     warnOrder :: Bool,
     warnDuplicate :: Bool,
     warnLineSkip :: Bool,
     warnIndentation :: Bool}
     -- Perhaps convert Column into (Row, Column) in the parser to accomodate.
  deriving (Eq, Ord, Read, Show)

defaultConfig :: Config
defaultConfig =
  Config
    {editor = Nothing,
     placeholder = "??",
     wrap = Just 80}

formatConfig :: Config -> String
formatConfig = show

-- This is dumb.
parseConfig :: String -> Either ExecutionError Config
parseConfig = left (const $ SyntaxError 0 0) . readEither

readConfig :: IO Config
readConfig =
  do fp <- getHomeDirectory
     c <- readFile $ fp </> projectConfig defaultProject
     case parseConfig c of
          Right q -> return q
          Left x -> throw x
