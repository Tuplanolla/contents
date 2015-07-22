{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

-- import Control.Arrow (first, second)
import Control.Applicative hiding (many, optional)
import Control.Exception hiding (try)
import Control.Monad
import Data.Map hiding (filter, partition)
import Data.List (isPrefixOf)
-- import Data.Text (Text)
import qualified Data.Text as T (justifyLeft, pack, unpack)
-- import qualified Data.Text.IO as TIO
import Data.Typeable
import Prelude hiding (foldl, getContents)
-- import System.Exit (exitFailure, exitSuccess)
import System.IO hiding (getContents)
import System.IO.Error
import System.Directory
import System.Exit
import System.Environment
import System.Process (rawSystem)
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec as P ((<|>))
import Text.Parsec.String (Parser)

data Wrapper =
  F (String -> Wrapper) |
  A Action
  deriving Show

type Wrapped = (Wrapper, Int)

data CommandError =
  Invalid {invalidInput :: String} |
  Ambiguous {ambiguousInput :: String, candidates :: [String]} |
  Incomplete {incompleteInput :: String, expected :: Int, actual :: Int}
  deriving (Show, Typeable)

instance Exception CommandError

wrap0 :: Action -> Wrapped
wrap0 a = (A a, 0)

wrap1 :: (String -> Action) -> Wrapped
wrap1 f = (F $ \ x -> A $ f x, 1)

wrap2 :: (String -> String -> Action) -> Wrapped
wrap2 f = (F $ \ x -> F $ \ y -> A $ f x y, 2)

commands :: [(String, Wrapped)]
commands =
  [("make", wrap0 Make),
   ("edit", wrap0 Edit),
   ("add", wrap2 Add),
   ("remove", wrap1 Remove),
   ("update", wrap2 Update),
   ("lookup", wrap1 Lookup),
   ("find", wrap1 Find),
   ("touch", wrap0 Touch),
   ("destroy", wrap0 Destroy),
   ("help", wrap0 Help),
   ("version", wrap0 Version)]

parseCommand :: [(String, a)] -> String -> Either CommandError a
parseCommand as x =
  case filter (isPrefixOf x . fst) as of
       ys @ (_ : _ : _) -> Left $ Ambiguous x $ fst <$> ys
       (y : _) -> Right $ snd y
       _ -> Left $ Invalid x

parseActionsWith :: Maybe ((String, Int), Wrapped) -> [String] -> Either CommandError [Action]
parseActionsWith (Just ((x, k), (F f, n))) (y : ys) =
  case f y of
       w @ (F _) -> parseActionsWith (Just ((x, k + 1), (w, n))) ys
       A a -> (a :) <$> parseActionsWith Nothing ys
parseActionsWith (Just ((x, k), (F _, n))) _ = Left $ Incomplete x n k
parseActionsWith (Just (_, (A a, _))) ys = (a :) <$> parseActionsWith Nothing ys
parseActionsWith _ (y : ys) =
  case parseCommand commands y of
       Right (w, n) -> parseActionsWith (Just ((y, 0), (w, n))) ys
       Left e -> Left e
parseActionsWith _ _ = Right []

parseActions :: [String] -> Either CommandError [Action]
parseActions = parseActionsWith Nothing

data Action =
  Make |
  Edit |
  Add {addKey :: String, addValue :: String} |
  Remove {removeKey :: String} |
  Update {updateKey :: String, updateValue :: String} |
  Lookup {lookupKey :: String} |
  Find {findKey :: String} |
  Touch |
  Destroy |
  Help |
  Version
  deriving Show

separatorParser :: Parser ()
separatorParser = () <$ char ' ' <* many1 (char ' ')

fileParser :: Parser (Map String String)
fileParser =
  fromList <$> many ((,) <$>
    (manyTill anyChar $ try $ separatorParser) <*>
    (manyTill anyChar $ try $ eof P.<|> () <$
    (newline <* notFollowedBy (() <$ try newline <|> try separatorParser))))

parseContents :: String -> Either ParseError (Map String String)
parseContents = runParser fileParser () []

-- This sucks.
sanitizeContents :: Map String String -> Map String String
sanitizeContents = mapBoth $ filter (/= '\n')

justifyLeft :: Int -> Char -> String -> String
justifyLeft n c = T.unpack . T.justifyLeft n c . T.pack

mapBoth :: Ord b => (a -> b) -> Map a a -> Map b b
mapBoth f = mapKeys f . Data.Map.map f

formatContents :: Map String String -> String
formatContents m =
  let xs = toAscList $ sanitizeContents m
      n = maximum $ length . fst <$> xs in
      unlines $ (\ (x, y) -> justifyLeft n ' ' x ++ "  " ++ y) <$> xs

data ExecutionError =
  AlreadyExists |
  NoEditor |
  EditorFailed {exitCode :: Int} |
  SyntaxError {row :: Int, column :: Int} |
  ContentError {commandError :: ParseError} |
  SwapInUse |
  Fuck
  deriving (Show, Typeable)

instance Exception ExecutionError

executeOne :: Configuration -> Action -> IO ()
executeOne c Make =
  catchJust -- This contains a possible race condition.
    (\ e -> if isDoesNotExistError e then Just () else Nothing)
    (do openFile (name c) ReadMode >>= hClose
        throw AlreadyExists)
    (\ _ -> openFile (name c) WriteMode >>= hClose)
executeOne c Edit =
  do m <- (<|>) <$> return (editor c) <*> lookupEnv "EDITOR" -- Check this.
     case m of
          Just x ->
            do e <- rawSystem x [name c]
               case e of
                    ExitFailure n -> throw $ EditorFailed n
                    _ -> return ()
          _ -> throw NoEditor
executeOne c (Add k v) = changeContents c $ insert k v
-- Not implemented yet.
executeOne c (Remove k) = (c, k) `seq` undefined
executeOne c (Update k v) = (c, k, v) `seq` undefined
executeOne c (Lookup k) = (c, k) `seq` undefined
executeOne c (Find k) = (c, k) `seq` undefined
executeOne c Touch = c `seq` undefined
executeOne c Destroy = c `seq` undefined
executeOne c Help = c `seq` undefined
executeOne c Version = c `seq` undefined

changeContents :: Configuration -> (Map String String -> Map String String) -> IO ()
changeContents c @ Configuration {name = file} f =
  do x <- readFile file
     _ <- evaluate $ length x
     case parseContents x of
          Right y ->
            do fp <- getCurrentDirectory
               fps <- getDirectoryContents fp
               -- Here goes something to merge reality with expectations.
               -- [FilePath] -> Map String String -> Map String (Maybe String, Bool)
               -- That Maybe says whether there is an entry and
               -- that Bool says whether a file exists.
               let z = formatContents $ f y
               case swap c of
                    Just g ->
                      let swapFile = g file in
                          catchJust -- This contains a possible race condition.
                            (\ e ->
                              if isDoesNotExistError e then
                                 Just () else
                                 Nothing)
                            (do h <- openFile swapFile ReadMode
                                hClose h
                                throw SwapInUse)
                            (\ _ ->
                              do writeFile swapFile z
                                 -- Assume the swap exists on the same volume.
                                 renameFile swapFile file)
                    _ -> writeFile file z
          Left e -> throw $ ContentError e

-- This is deprecated.
readContents :: Configuration -> IO (Map String String)
readContents c =
  do x <- readFile $ name c
     _ <- evaluate $ length x
     case parseContents x of
          Right y -> return y
          Left e -> throw $ ContentError e

-- This is deprecated.
writeContents :: Configuration -> Map String String -> IO ()
writeContents c y =
  case swap c of
       Just f ->
         -- Check swap file existence.
         do writeFile (f $ name c) $ formatContents y
            -- Assume swap exists on the same volume.
            renameFile (f $ name c) (name c)
       _ -> writeFile (name c) $ formatContents y

data Configuration =
  Configuration
    {name :: String,
     editor :: Maybe String,
     swap :: Maybe (String -> String),
     skip :: Maybe Int,
     wrap :: Maybe Int}
  deriving Show

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    {name = "CONTENTS",
     editor = Nothing,
     swap = Just $ \ x -> "." ++ x ++ ".swap",
     skip = Just 20,
     wrap = Just 80}

execute :: Configuration -> [Action] -> IO ()
execute c = mapM_ $ executeOne c

testc :: Either CommandError [Action]
testc = parseActions ["make", "to", "add", "key", "value", "look", "key"]

testp :: IO (Either ParseError (Map String String))
testp = parseContents <$> readFile (name defaultConfiguration)

testf :: IO (Either ParseError String)
testf = fmap formatContents <$> testp

testq :: IO ()
testq =
  do x <- testf
     case x of
          Right y -> putStr y
          Left e -> print e

mainWith :: [String] -> IO ()
mainWith xs =
  case parseActions xs of
       Right x ->
         do c <- return defaultConfiguration -- Read here.
            execute c x
       Left x -> print x

main :: IO ()
main =
  do xs <- getArgs
     mainWith xs
