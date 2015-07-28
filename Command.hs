module Command where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)

import Error

data Action =
  -- | Create an empty table of contents if none exists.
  Create |
  -- | Fill the table of contents based on currently existing files.
  -- Interactive if possible, asks about each file.
  -- Leave the value empty to skip.
  Interact |
  -- | Edit file in text editor, automatically sanitize afterwards.
  Edit |
  -- | Add a new entry or fail for an existing one.
  Add {addKey :: String, addValue :: String} |
  Remove {removeKey :: String} |
  Update {updateKey :: String, updateValue :: String} |
  -- | Retrieve an entry with the exact key.
  Lookup {lookupKey :: String} |
  -- | Retrieve an entry based on any part of the key.
  Find {findEntry :: String} |
  FindKey {findKey :: String} |
  FindValue {findValue :: String} |
  -- | Just run the sanitizer.
  Touch |
  -- | Delete the table of contents file.
  Destroy |
  Help |
  Version
  deriving (Eq, Ord, Read, Show)

data ConfigAction =
  -- | Edit config file in text editor, check afterwards.
  EditConfig |
  Destroy |
  Help |
  Version
  deriving (Eq, Ord, Read, Show)

key :: Action -> Maybe String
key (Add k _) = Just k
key (Remove k) = Just k
key (Update k _) = Just k
key (Lookup k) = Just k
key (Find k) = Just k
key _ = Nothing

value :: Action -> Maybe String
value (Add _ v) = Just v
value (Update _ v) = Just v
value _ = Nothing

data Wrapper =
  F (String -> Wrapper) |
  A Action

type Wrapped = (Wrapper, Int)

wrap0 :: Action -> Wrapped
wrap0 a = (A a, 0)

wrap1 :: (String -> Action) -> Wrapped
wrap1 f = (F $ \ x -> A $ f x, 1)

wrap2 :: (String -> String -> Action) -> Wrapped
wrap2 f = (F $ \ x -> F $ \ y -> A $ f x y, 2)

-- Actually Map String Wrapped.
commands :: [(String, Wrapped)]
commands =
  [("create", wrap0 Create),
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

parseCommand :: [(String, a)] -> String -> Either ExecutionError a
parseCommand as x =
  let f (y, _) = x `isPrefixOf` y in
      case filter f as of
           ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
           (y : _) -> Right $ snd y
           _ -> Left $ Invalid x

parseActionsWith ::
  Maybe ((String, Int), (Wrapper, Int)) ->
  [String] -> Either ExecutionError [Action]
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

parseActions :: [String] -> Either ExecutionError [Action]
parseActions = parseActionsWith Nothing
