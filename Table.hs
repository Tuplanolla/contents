module Table where

import Control.Applicative ((*>), (<$), (<$>), (<*), (<*>))
import Control.Arrow (first, left)
import Data.Function
import Data.List as L
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

data Marked a =
  File a |
  Directory a
  deriving (Eq, Ord, Read, Show)

getThing (File x) = x
getThing (Directory x) = x

data Row a =
  Row {getRowQ :: Int, getAQ :: a}
  deriving (Eq, Ord, Read, Show)

data RowColumn a =
  RowColumn {getRow :: Int, getColumn :: Int, getA :: a}
  deriving (Eq, Ord, Read, Show)

-- This works somehow.
t :: Parser [(Row (Marked String), [RowColumn String])]
t = many e
e :: Parser (Row (Marked String), [RowColumn String])
e = (,) <$> k <*> v
k :: Parser (Row (Marked String))
k =
  do _ <- notFollowedBy w
     _ <- notFollowedBy d
     n <- sourceLine <$> getPosition
     y <- manyTill anyChar (lookAhead (try (s *> s) <|> l <|> d))
     f <- Directory <$ d <|> return File
     return $ Row n $ f y
v :: Parser [RowColumn String]
v = s *> s *> c <|> l *> p
c :: Parser [RowColumn String]
c =
  do _ <- many s
     n <- sourceLine <$> getPosition
     k <- sourceColumn <$> getPosition
     _ <- notFollowedBy w
     y <- manyTill (notFollowedBy (w *> l) *> anyChar) (lookAhead l)
     _ <- l
     ys <- z <|> return []
     return $ RowColumn n k y : ys
p :: Parser [RowColumn String]
p = s *> c <|> l *> p
z :: Parser [RowColumn String]
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

parseTable :: String -> Either ExecutionError [(Row (Marked String), [RowColumn String])]
parseTable = left mapError . runParser (t <* eof) () "<anonymous>"

checkLineSkipIn :: [Int] -> [ExecutionWarning]
checkLineSkipIn ns =
  case nub ns of
       _ : [] -> []
       _ -> [WLineSkip]

checkLineSkip :: [(Row (Marked String), [RowColumn String])] -> [ExecutionWarning]
checkLineSkip xs =
  let f (RowColumn n _ _) = n
      lastLine (_, vs) = maximum (f <$> vs)
      firstLine (Row n _, _) = n
      ys = sortBy (comparing firstLine) xs in
      checkLineSkipIn $ (zipWith subtract . tail) (firstLine <$> ys) (lastLine <$> ys)

checkIndentationIn :: [RowColumn String] -> [ExecutionWarning]
checkIndentationIn xs =
  case nubBy ((==) `on` getColumn) xs of
       _ : [] -> []
       _ -> [WIndentation]

checkIndentation :: [(Row (Marked String), [RowColumn String])] -> [ExecutionWarning]
checkIndentation xs = checkIndentationIn $ snd `concatMap` xs

checkDuplicate :: [(Row (Marked String), [RowColumn String])] -> [ExecutionWarning]
checkDuplicate xs =
  let ys = getThing . getAQ . fst <$> xs
      zs = sort ys in
      if nub zs == zs then
         [] else
         [WDuplicate]

checkOrder :: [(Row (Marked String), [RowColumn String])] -> [ExecutionWarning]
checkOrder xs =
  let ys = getThing . getAQ . fst <$> xs in
      if sort ys == ys then
         [] else
         [WOrder]

checkAll :: [(Row (Marked String), [RowColumn String])] -> [ExecutionWarning]
checkAll xs =
  ($ xs) `concatMap`
    [checkLineSkip, checkIndentation, checkDuplicate, checkOrder]

stripPositions :: [(Row (Marked String), [RowColumn String])] -> [(Marked String, [String])]
stripPositions xs =
  let f (RowColumn _ _ v) = v
      g (Row _ k, vs) = (k, f <$> vs) in
      g <$> xs

-- This is silly and should remain internal.
mergeTable :: [(String, [String])] -> [(String, [String])]
mergeTable xs =
  let f xs @ ((x, _) : _) = (x, concat $ snd <$> xs)
      ys = groupBy ((==) `on` fst) $ sortBy (comparing fst) xs in
      f <$> ys

-- It does some extra work and is kind of really stupid, but... gives results.
cleanTable :: [(Marked String, [String])] -> Map String String
cleanTable xs =
  let f (_, ys) = unwords ys
      ys = mergeTable $ first getThing <$> xs in
      fromList $ zip (fst <$> ys) (f <$> ys)

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
