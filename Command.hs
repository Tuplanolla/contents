module Command where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)

import Error

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

data Wrapper =
  F (String -> Wrapper) |
  A Action
  -- deriving Show

type Wrapped = (Wrapper, Int)

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

parseActionsWith ::
  Maybe ((String, Int), Wrapped) -> [String] -> Either CommandError [Action]
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
