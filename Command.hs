module Command where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Data.Map

import Error

data Action a =
  -- | Create an empty table of contents if none exists.
  Create |
  -- | Fill the table of contents based on currently existing files.
  -- Interactive if possible, asks about each file.
  -- Leave the value empty to skip.
  Interact |
  -- | Edit file in text editor, automatically sanitize afterwards.
  Edit |
  -- | Add a new entry or fail for an existing one.
  Add {addKey :: a, addValue :: a} |
  Remove {removeKey :: a} |
  Update {updateKey :: a, updateValue :: a} |
  -- | Retrieve an entry with the exact key.
  Lookup {lookupKey :: a} |
  -- | Retrieve an entry based on any part of the key.
  Find {findEntry :: a} |
  FindKey {findKey :: a} |
  FindValue {findValue :: a} |
  -- | Just run the sanitizer.
  Touch |
  -- | Delete the table of contents file.
  Destroy |
  Help |
  Version
  deriving (Eq, Ord, Read, Show)

key :: Action a -> Maybe a
key (Add k _) = Just k
key (Remove k) = Just k
key (Update k _) = Just k
key (Lookup k) = Just k
key (Find k) = Just k
key _ = Nothing

value :: Action a -> Maybe a
value (Add _ v) = Just v
value (Update _ v) = Just v
value _ = Nothing

data Wrapper a =
  F (a -> Wrapper a) |
  A (Action a)

type Wrapped a = (Wrapper a, Int)

wrap0 :: Action a -> (Wrapper a, Int)
wrap0 a = (A a, 0)

wrap1 :: (a -> Action a) -> (Wrapper a, Int)
wrap1 f = (F $ \ x -> A $ f x, 1)

wrap2 :: (a -> a -> Action a) -> (Wrapper a, Int)
wrap2 f = (F $ \ x -> F $ \ y -> A $ f x y, 2)

commands :: Map String (Wrapper a, Int)
commands =
  fromList
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
     -- The config system actually has the same commands!

parseCommand :: Map String b -> String -> Either ContentsError b
parseCommand as x =
  let f k _ = x `isPrefixOf` k in
      case toAscList $ filterWithKey f as of
           ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
           (y : _) -> Right $ snd y
           _ -> Left $ Invalid x

parseActionsWith ::
  Maybe ((String, Int), (Wrapper String, Int)) ->
  [String] -> Either ContentsError [Action String]
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

parseActions :: [String] -> Either ContentsError [Action String]
parseActions = parseActionsWith Nothing
