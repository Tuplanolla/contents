{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

module Extra where

import Data.Map (Map, map, mapKeys)
import Text.Parsec (ParsecT, Stream, manyTill, notFollowedBy)

mapBoth :: Ord b => (a -> b) -> Map a a -> Map b b
mapBoth f = mapKeys f . Data.Map.map f

-- Not the best of fixes...
many1Till ::
  (Stream s m t, Show end) =>
  ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end =
  do notFollowedBy end
     first <- p
     rest <- manyTill p end
     return $ first : rest
