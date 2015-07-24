{-# LANGUAGE FlexibleContexts #-}

module Extra where

import Control.Applicative ((<$), (<$>), (<*), (<*>))
import Data.Map as Map (Map)
import qualified Data.Map as Map
import System.Environment
import Text.Parsec

-- Data.Map
mapBoth :: Ord b => (a -> b) -> Map a a -> Map b b
mapBoth f = Map.map f . Map.mapKeys f

-- System.Environment
getEditor :: IO (Maybe FilePath)
getEditor = lookupEnv "EDITOR"

-- Text.Parsec.Char
lineTerminator :: Stream s m Char => ParsecT s u m String
lineTerminator =
  try (string "\r\n") <|>
  string "\r" <|>
  string "\n" <|>
  "" <$ lookAhead eof

-- Text.Parsec.Combinator
manyN :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
manyN n p
  | n < 1 = many p
  | otherwise = (:) <$> p <*> manyN (n - 1) p

-- Text.Parsec.Combinator
many1Till ::
  (Show end, Stream s m t) =>
  ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p q =
  do notFollowedBy q
     (:) <$> p <*> manyTill p q

-- Text.Parsec.Combinator
manyNTill ::
  (Show end, Stream s m t) =>
  Int -> ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyNTill n p q
  | n < 1 = manyTill p q
  | otherwise =
      do notFollowedBy q
         (:) <$> p <*> manyNTill (n - 1) p q

-- Text.Parsec.Combinator
manyTo ::
  Stream s m t =>
  ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTo p q =
  let r = [] <$ lookAhead q <|> (:) <$> p <*> r in
      r
