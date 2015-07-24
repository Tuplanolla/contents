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

b :: Parser [(String, [(Int, String)])]
b = e <|> i u' *> (s *> s *> c <|> n *> h)

i x = (:) <$> noneOf " \r\n" <*> manyUntil anyChar (try x)

g x = n *> x <|> many1 s *> d

t' = r <|> n

h = g h

u' = s *> s <|> r <|> n

n = char '\n'

d = i t' *> n *> (a <|> b)

e = [] <$ eof

r = char '\r'

c = many s *> d

a = g (a <|> b)

s = char ' '

-- manyTill that does not consume end
manyUntil :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyUntil p end      = scan
                    where
                      scan  = do{ lookAhead end; return [] }
                            <|> do{ x <- p; xs <- scan; return (x:xs) }

parseContents :: String -> Either ParseError [(String, [(Int, String)])]
parseContents = runParser b () []

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
