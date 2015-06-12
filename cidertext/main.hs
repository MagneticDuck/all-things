module Main where

import Data.List
import System.IO.Unsafe
import Data.Ord
import Data.Char

import Pattern

english :: String
english = init . unsafePerformIO $ readFile "english"

cacheName :: String
cacheName = "english.cache"

updateCache :: IO ()
updateCache = writeCache cacheName 4 (take 10000 english)

cache :: FrequencyCache
cache = readCache cacheName

myLookup :: String -> [(Integer, String)]
myLookup str =
  take 5 $ lookupPattern cache (makePattern str)

-- getMatches :: String -> [(Integer, String)]
-- getMatches str = checkPattern (makePattern str) english

main :: IO ()
main = putStrLn "testing"
