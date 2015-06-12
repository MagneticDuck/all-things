module CWData.Processing where

-- this module contains some various tools that I used to collect information. 
-- Don't expect to use it out of the box, because it reads from various files in the WD.

import CWData.Data
import Control.Applicative
import System.IO.Unsafe
import Data.List
import Data.Ord

maps :: [Map]
maps = unsafePerformIO getMaps

mapCount :: Int
mapCount = length maps

authors :: [String]
authors = nub . map _mapAuthor $ maps

mapPlayers :: String -> IO [String]
mapPlayers = (fmap . map) _scoreUser . getScores 

mapPlayerRanks :: String -> IO [(String, Int)]
mapPlayerRanks = (fmap . map) (\score -> (_scoreUser score, _scoreRank score)) . getScores

allPlayers :: [String]
allPlayers =
  map fst playerRanks

writePlayersToFile :: FilePath -> IO ()
writePlayersToFile filepath =
  (`mapM_` (zip (map _mapUid maps) [1..]))
    (\(uid, i) ->
      do
        print i
        appendFile filepath =<< (fmap unlines $ mapPlayers uid))

writePlayerRanksToFile :: FilePath -> IO ()
writePlayerRanksToFile filepath =
  (`mapM_` (zip (map _mapUid maps) [1..]))
    (\(uid, i) ->
      do
        print i
        appendFile filepath =<< (unlines . map show <$> mapPlayerRanks uid))

playerRanks :: [(String, Int)]
playerRanks = unsafePerformIO $ (map read . lines) <$> readFile "./playerRanks"

mapsByFlash :: [Map]
mapsByFlash =
  filter ((== "Flash1225") . _mapAuthor) maps

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

playerStats :: [(String, Int)]
playerStats =
  map (\player -> (player, length . filter (== player) $ allPlayers)) $ nub allPlayers

histogram :: [Int]
histogram = map (\i -> length . filter ((== i) . snd) $ playerStats) []

numberOnePlacesByName :: String -> Int
numberOnePlacesByName name =
  length . filter ((== 1) . snd) . filter ((== name) . fst) $ playerRanks

top10 :: [String]
top10 = 
  map fst . reverse . take 10 . reverse $ 
    sortBy (comparing snd) playerStats

authorRatings :: String -> [Float]
authorRatings author =
  map _mapRating $ filter ((== author) . _mapAuthor) maps

authorMaps :: String -> Int
authorMaps author =
  length $ filter ((== author) . _mapAuthor) maps
