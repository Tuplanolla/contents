module Table where

import Control.Applicative ((*>), (<$), (<$>), (<*), (<*>))
import Control.Arrow (left)
import Data.Function
import Data.List
import Data.Map as M
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String (Parser)
import qualified Data.Text as T (justifyLeft, pack, unpack)

import Config
import Error
import Extra

-- This works somehow.
t :: Parser [(String, [(Int, String)])]
t = many e
e :: Parser (String, [(Int, String)])
e = (,) <$> k <*> v
k :: Parser String
k =
  do _ <- notFollowedBy w
     _ <- notFollowedBy d
     y <- manyTill anyChar (lookAhead (try (s *> s) <|> l <|> d))
     _ <- d <|> return []
     return y
v :: Parser [(Int, String)]
v = s *> s *> c <|> l *> p
c :: Parser [(Int, String)]
c =
  do _ <- many s
     n <- sourceColumn <$> getPosition
     _ <- notFollowedBy w
     y <- manyTill (notFollowedBy (w *> l) *> anyChar) (lookAhead l)
     _ <- l
     ys <- z <|> return []
     return $ (n, y) : ys
p :: Parser [(Int, String)]
p = s *> c <|> l *> p
z :: Parser [(Int, String)]
z = s *> c <|> l *> (z <|> return [])
w :: Parser String
w = l <|> s
l :: Parser String
l = (++) <$> r <*> n <|> r <|> n
d :: Parser String
d = f <|> b
s :: Parser String
s = string " "
r :: Parser String
r = string "\r"
n :: Parser String
n = string "\n"
f :: Parser String
f = string "/"
b :: Parser String
b = string "\\"

mapError :: ParseError -> ExecutionError
mapError e =
  let p = errorPos e in
      SyntaxError (sourceLine p) (sourceColumn p)

parseTable :: String -> Either ExecutionError [(String, [(Int, String)])]
parseTable = left mapError . runParser (t <* eof) () []

-- This is silly and should remain internal.
mergeTable :: [(String, [(Int, String)])] -> [(String, [(Int, String)])]
mergeTable xs =
  let f xs @ ((x, _) : _) = (x, concat $ snd <$> xs)
      ys = groupBy ((==) `on` fst) $ sortBy (comparing fst) xs in
      f <$> ys

-- It does some extra work and is kind of really stupid, but... gives results.
cleanTable :: [(String, [(Int, String)])] -> Map String String
cleanTable xs =
  let f (x, ys) = unwords $ snd <$> ys
      ys = mergeTable xs in
      fromList $ zip (fst <$> ys) (f <$> ys)

-- When encountering incorrect indentation:
-- "This does not seem to be a table of contents file. Keep going?"

-- Merge entries with the same key.

justifyLeft :: Int -> String -> String
justifyLeft n = T.unpack . T.justifyLeft n ' ' . T.pack

indentLeft :: Int -> String -> String
indentLeft n xs
  | n > 0 = indentLeft (n - 1) $ ' ' : xs
  | otherwise = xs

-- This is baked into the grammar, but not separated there yet.
dropTrails :: Map String String -> Map String String
dropTrails m =
  let f xs @ (x : [])
        | x == '/' || x == '\\' = []
        | otherwise = xs
      f (x : xs) = x : f xs
      f _ = [] in
      f `mapKeys` m

-- There is no wrapping yet and the logic is kind of shit too.
formatTable :: Config -> Map String String -> String
formatTable c m =
  let xs = toAscList m
      n = maximum $ length . fst <$> xs
      g (x, y) =
        if True then
           justifyLeft (n + 2) x ++ y else
           x ++ "\n" ++ justifyLeft (max 1 ((\ (Just x) -> x) $ Just 5) - 1) " " ++ y in
           unlines $ g <$> xs

data Value =
  Matching String |
  Missing |
  Spurious String
  deriving (Eq, Ord, Read, Show)

-- Extend with Data.Set.zipWith to define this better.
wrangleTable :: Set FilePath -> Map String String -> Map String Value
wrangleTable s m =
  let f Missing (Spurious x) = Matching x in
      unionWith f (fromSet (const Missing) s) (Spurious <$> m)

fromWrangledTable :: Config -> Map String Value -> Map String String
fromWrangledTable c m =
  -- These depend on the Config.
  let -- f Missing = False
      f _ = True
      g (Matching x) = x
      -- Make sure that placeholder is not empty or
      -- the file will be corrupted gently.
      g Missing = placeholder c
      g (Spurious x) = x in
      g <$> M.filter f m

-- Just for developer convenience.
handOverTable :: String -> Either ExecutionError (Map String String)
handOverTable x = cleanTable <$> parseTable x
