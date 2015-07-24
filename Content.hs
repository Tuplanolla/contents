module Content where

import Control.Applicative ((*>), (<$), (<$>), (<*), (<*>))
import Data.Map (Map, toAscList, fromList)
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String (Parser)
import qualified Data.Text as T (justifyLeft, pack, unpack)

import Extra

-- When encountering incorrect indentation:
-- "This does not seem to be a table of contents file. Keep going?"

-- Merge entries with the same key.

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

parseContents :: String -> Either ParseError [(String, [(Int, String)])]
parseContents = runParser (c <* eof) () []

-- This sucks.
sanitizeContents :: Map String String -> Map String String
sanitizeContents = mapBoth $ filter (/= '\n')

justifyLeft :: Int -> Char -> String -> String
justifyLeft n c = T.unpack . T.justifyLeft n c . T.pack

formatContents :: Map String String -> String
formatContents m =
  let xs = toAscList $ sanitizeContents m
      n = maximum $ length . fst <$> xs in
      unlines $ (\ (x, y) -> justifyLeft n ' ' x ++ "  " ++ y) <$> xs
