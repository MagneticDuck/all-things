module Pattern
  ( 
  -- Pattern: a structure symmetric under permutation of the alphabet {{{
  Pattern
  -- constructors
  , makePattern
  -- accessors
  , isOfPattern
  -- }}}

  -- FrequencyCache: a cached data structure that statistically describes a language {{{
  , FrequencyCache
  , writeCache
  , readCache
  , lookupPattern
  -- }}}


  ) where

import Data.Maybe
import Data.List
import Data.Ord
import System.IO.Unsafe

-- Pattern {{{
newtype Pattern = Pattern {getPattern :: [Int]} deriving (Eq)

instance Show Pattern where
  show = intercalate " " . map show . getPattern

makePattern :: (Eq a) => [a] -> Pattern 
makePattern xs = 
  Pattern $ foldl f [1] (drop 1 xs)
  where
    f acc x =
      case find ((== x) . fst) (zip xs acc) of
        Nothing -> 
          acc ++ [(succ (maximum acc))]
        Just (_, y) -> acc ++ [y]

isOfPattern :: (Eq a) => Pattern -> [a] -> Bool
isOfPattern pat xs =
  makePattern xs == pat

patternLength :: Pattern -> Int
patternLength = length . getPattern
-- }}}

-- FrequencyCache {{{

count :: (Eq a) => [a] -> [(Integer, a)]
count xs =
  map (\x -> (toInteger $ length (filter (== x) xs), x)) $ nub xs

splitPieces :: Int -> [a] -> [[a]]
splitPieces i xs =
  map (\ai -> take i . drop ai $ xs) $ [0, 1..(length xs - i)]

commonSeqs :: (Eq a) => Int -> [a] -> [(Integer, [a])]
commonSeqs i xs = reverse . sortBy (comparing fst) . count . splitPieces i $ xs

type FrequencyCache = [(Int, [(Integer, String)])] 

writeCache :: FilePath -> Int -> String -> IO ()
writeCache path order str =
  writeFile path "" >> sequence_ operations
  where
    operations :: [IO ()]
    operations =
      concat [
        [appendFile path "[\n"]
        , intersperse writeComma $ map writeLevel [1..order]
        , [appendFile path "\n]"]
      ]
    writeComma = appendFile path "\n,\n"
    writeLevel i = do
      putStrLn $ "writing level " ++ show i ++ "..."
      appendFile path . show $
        (i, take 2000 (commonSeqs i str))

readCache :: FilePath -> FrequencyCache
readCache = unsafePerformIO . fmap read . readFile

lookupPattern :: FrequencyCache -> Pattern -> [(Integer, String)]  
lookupPattern cache pat =
  case find ((== len) . fst) cache of
    Nothing -> []
    Just (_, common) -> filter (isOfPattern pat . snd) $ common
  where
    len = patternLength pat

-- }}}
