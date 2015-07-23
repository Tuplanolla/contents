module Content where

import Control.Applicative ((<$>), (<*), (<*>))
import Data.Map (Map, toAscList, fromList)
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String (Parser)
import qualified Data.Text as T (justifyLeft, pack, unpack)

import Extra

separator :: Parser String
separator = manyN 2 $ char ' '

-- When encountering incorrect indentation:
-- "This does not seem to be a table of contents file. Keep going?"

-- Potential problems:
-- start the file with "key···\n" or "··value\n"

-- Merge entries with the same key.

line :: Parser (Int, String)
line =
  do n <- sourceColumn <$> getPosition
     v <- many1Till anyChar $ try (lookAhead lineTerminator)
     return (n, v)

entry :: Parser (String, [(Int, String)])
entry =
  do notFollowedBy $ char ' '
     k <- many1Till anyChar $ try separator
     vs <- many1Till line $ try $ lineTerminator <* notFollowedBy separator
     return (k, vs)

file :: Parser [(String, [(Int, String)])]
file = many entry

parseContents :: String -> Either ParseError [(String, [(Int, String)])]
parseContents = runParser (file <* eof) () []

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
