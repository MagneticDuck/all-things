module CWData.Data 
-- exports -- {{{
  ( Map(..)
  , mapIndex, mapDownloads, mapScoreCount, mapRatingCount
  , mapUid, mapAuthor, mapTitle, mapDescription, mapRating
  , Score(..)
  , scoreUser, scoreValue, scoreRank, scoreTime, scorePlays
  , Scores
  , getMaps
  , getScores )
    where -- }}}
 
-- imports -- {{{
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import System.IO

import System.Process

import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

import Lens.Family2
import Lens.Family2.TH -- }}}

-- Map -- {{{
data Map = 
  Map
    { _mapIndex :: Int -- i
    , _mapUid :: String -- g
    , _mapAuthor :: String -- a
    , _mapTitle :: String -- l
    , _mapDescription :: T.Text -- e
    , _mapDownloads :: Int  -- o
    , _mapScoreCount :: Int -- s
    , _mapRatingCount :: Int -- n
    , _mapRating :: Float -- r
    } deriving (Eq, Show) -- }}}

-- Score -- {{{
data Score =
  Score
    { _scoreUser :: String
    , _scoreRank :: Int
    , _scoreValue :: Int
    , _scoreTime :: Int
    , _scorePlays :: Int } 
    deriving (Show, Eq)

type Scores = [Score]    
    
  -- }}}

-- Lenses -- {{{
$(makeLenses ''Map)
mapIndex, mapDownloads, mapScoreCount, mapRatingCount :: Functor f => LensLike' f Map Int
mapUid, mapAuthor, mapTitle :: Functor f => LensLike' f Map String
mapDescription :: Functor f => LensLike' f Map T.Text
mapRating :: Functor f => LensLike' f Map Float
$(makeLenses ''Score)
scoreUser :: Functor f => LensLike' f Score String
scoreValue, scoreRank, scoreTime, scorePlays :: Functor f => LensLike' f Score Int -- }}}

readFileWithLatin1 :: String -> IO T.Text -- {{{
readFileWithLatin1 filepath =
  do
    h <- openFile filepath ReadMode
    hSetEncoding h latin1
    T.pack <$> hGetContents h  -- }}}


requestMaps :: IO T.Text  -- {{{
requestMaps =
  do
    void $ system "wget -O /tmp/cwdata.gz http://knucklecracker.com/creeperworld3/queryMaps.php?query=maplist &> /dev/null" 
    void $ system "gzip -df /tmp/cwdata.gz"
    readFileWithLatin1 "/tmp/cwdata" -- }}}

extractMaps :: T.Text -> [T.Text] -- {{{
extractMaps t =
  case parseOnly parser t of
    Left str -> error str
    Right result -> map T.pack result
  where
    parser = 
      do
        void $ string "<maps>"
        result <- many1 . try $ string "<m>" *> manyTill anyChar (try $ string "</m>")
        void $ string "</maps>"
        return result -- }}}

parseContents :: T.Text -> [(String, String)] -- {{{
parseContents t =
  case parseOnly parser t of
    Left str -> error str
    Right result -> result
  where
    parser = many1 contentParser -- }}}

contentParser :: Parser (String, String) -- {{{
contentParser =
  do
    a <- char '<' >> many1 (notChar '>') <* char '>'
    b <- manyTill anyChar (try . string . T.pack $ "</" ++ a ++ ">")
    return (a, b)  -- }}}

parseMap :: T.Text -> Map -- {{{
parseMap t = 
  foldl (.) id (map f $ parseContents t) initMap
  where
    f (key, value) =
      ($ value) $
        case key of
          "i" -> (mapIndex .~) . read 
          "g" -> (mapUid .~)
          "a" -> (mapAuthor .~)
          "l" -> (mapTitle .~)
          "e" -> (mapDescription .~) . T.pack
          "o" -> (mapDownloads .~) . read
          "s" -> (mapScoreCount .~) . read
          "n" -> (mapRatingCount .~) . read
          "r" -> (mapRating .~) . read
          _   -> const id
    initMap =
      Map
        { _mapIndex = 1 , _mapUid = "" , _mapAuthor = "" , _mapTitle = ""
        , _mapDescription = "", _mapDownloads = 0, _mapScoreCount = 0
        , _mapRatingCount = 0, _mapRating = 0} -- }}}

getMaps :: IO [Map] 
getMaps = map parseMap . extractMaps <$> requestMaps 

requestScores :: String -> IO T.Text -- {{{
requestScores uid =
  do
    void . system $ "wget -O /tmp/cwscores http://knucklecracker.com/creeperworld3/scoreQuery.php?gameUID=" ++ uid  ++ " &> /dev/null" 
    readFileWithLatin1 "/tmp/cwscores" -- }}}

extractScores :: T.Text -> [T.Text] -- {{{
extractScores t =
  case parseOnly parser t of
    Left str -> error str
    Right result -> map T.pack result
  where
    parser = 
      do
        void $ string "<records>"
        result <- many1 . try $ string "<record>" *> manyTill anyChar (try $ string "</record>")
        void $ string "</records>"
        return result -- }}}

parseScore :: T.Text -> Score -- {{{
parseScore t =
    foldl (.) id (map f $ parseContents t) initScore
  where
    f :: (String, String) -> Score -> Score
    f (key, value) =
      ($ value) $ 
        case key of
          "rank"  -> (scoreRank .~) . read
          "user"  -> (scoreUser .~)
          "score" -> (scoreValue .~) . read
          "time"  -> (scoreTime .~) . read
          "plays" -> (scorePlays .~) . read
          _       -> const id
    initScore =
      Score 
        { _scoreValue = 0, _scoreRank = 1, _scoreUser = "", _scoreTime = 0, _scorePlays = 0} 
        -- }}}

getScores :: String -> IO Scores
getScores = ((map parseScore . extractScores) <$>) . requestScores 
