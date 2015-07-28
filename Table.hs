module Table where

import Control.Applicative ((*>), (<$), (<$>), (<*), (<*>))
import Control.Arrow (left)
import Data.Function
import Data.List
import Data.Map (Map, toAscList, fromList)
import Data.Ord
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String (Parser)
import qualified Data.Text as T (justifyLeft, pack, unpack)

import Config
import Error
import Extra

-- This works somehow.
c :: Parser [(String, [(Int, String)])]
c = many e
e :: Parser (String, [(Int, String)])
e = (,) <$> k <*> v
k :: Parser String
k =
  do _ <- notFollowedBy w
     y <- manyTill anyChar (lookAhead (try (s *> s) <|> l))
     return y
v :: Parser [(Int, String)]
v = s *> s *> x <|> l *> y
x :: Parser [(Int, String)]
x =
  do _ <- many s
     n <- sourceColumn <$> getPosition
     _ <- notFollowedBy w
     y <- manyTill (notFollowedBy (w *> l) *> anyChar) (lookAhead l)
     _ <- l
     ys <- z <|> return []
     return $ (n, y) : ys
y :: Parser [(Int, String)]
y = s *> x <|> l *> y
z :: Parser [(Int, String)]
z = s *> x <|> l *> (z <|> return [])
w :: Parser String
w = l <|> s
l :: Parser String
l = (++) <$> r <*> n <|> r <|> n
s :: Parser String
s = string " "
r :: Parser String
r = string "\r"
n :: Parser String
n = string "\n"

mapError :: ParseError -> ExecutionError
mapError e =
  let p = errorPos e in
      SyntaxError (sourceLine p) (sourceColumn p)

parseTables :: String -> Either ExecutionError [(String, [(Int, String)])]
parseTables = left mapError . runParser (c <* eof) () []

-- This is silly and should remain internal.
mergeTables :: [(String, [(Int, String)])] -> [(String, [(Int, String)])]
mergeTables xs =
  let f xs @ ((x, _) : _) = (x, concat $ snd <$> xs)
      ys = groupBy ((==) `on` fst) $ sortBy (comparing fst) xs in
      f <$> ys

-- It does some extra work and is kind of really stupid, but... gives results.
cleanTables :: [(String, [(Int, String)])] -> Map String String
cleanTables xs =
  let f (x, ys) = unwords $ snd <$> ys
      ys = mergeTables xs in
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

-- There is no wrapping yet and the logic is kind of shit too.
formatTables :: Config -> Map String String -> String
formatTables c m =
  let xs = toAscList m
      n = maximum $ length . fst <$> xs
      g (x, y) =
        if True then
           justifyLeft (n + 2) x ++ y else
           x ++ "\n" ++ justifyLeft (max 1 ((\ (Just x) -> x) $ Just 5) - 1) " " ++ y in
           unlines $ g <$> xs

-- Just for developer convenience.
handOverTables :: String -> Either ExecutionError (Map String String)
handOverTables x = cleanTables <$> parseTables x
