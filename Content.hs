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
b = [] <$ eof <|>
  do x0 <- h
     x1 <-
       string "  " *> c <|>
       l *> p <|>
       do key <- u
          (val, rest) <-
            string "  " *> c <|>
            l *> p
          return $ (key, val) : rest
c, p, d :: Parser ([(Int, String)], [(String, [(Int, String)])])
c =
  do _ <- many (char ' ')
     n <- sourceColumn <$> getPosition
     val <- d
     return (n, val)
d =
  do x0 <- h
     x1 <- l <|>
       (\ xs x -> xs ++ [x]) <$> manyTo t (try (h <* l)) <*> h <* l
     x2 <- a <|> b
     return x
p = l *> p <|> many1 (char ' ') *> d
a = l *> (a <|> b) <|> many1 (char ' ') *> d
l = try (string "\r\n") <|> string "\r" <|> string "\n" <|>
u = manyTo t (try (string "  ") <|> oneOf "\r\n")
h = noneOf " \r\n"
t = noneOf "\r\n"

-- manyTill that does not consume end
manyTo :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTo p end      = scan
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
