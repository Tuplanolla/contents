{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

module Content where

import Control.Applicative ((<$), (<$>), Alternative((<|>)), Applicative((<*), (<*>)))
import Data.Map (Map, toAscList, fromList)
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec as P ((<|>))
import Text.Parsec.String (Parser)
import qualified Data.Text as T (justifyLeft, pack, unpack)

import Extra

separatorParser :: Parser ()
separatorParser = () <$ char ' ' <* many1 (char ' ')

-- When encountering incorrect indentation:
-- "This does not seem to be a table of contents file. Keep going?"

-- Potential problems:
-- start the file with "key···\n" or "··value\n"

-- Merge entries with the same key.

fileParser :: Parser (Map String String)
fileParser =
  fromList <$> many ((,) <$>
    (many1Till anyChar $ try $ separatorParser) <*>
    (many1Till anyChar $ try $ eof P.<|> () <$
    (newline <* notFollowedBy (() <$ try newline <|> try separatorParser))))

parseContents :: String -> Either ParseError (Map String String)
parseContents = runParser (fileParser <* eof) () []

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
