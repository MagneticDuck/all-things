module SayNumber where

import Data.List (find, intersperse, intercalate)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)

type SayMaybe a = a -> Maybe String

-- given a predicate to decide whether a supplied value is in a set of numbers that contains
-- all numbers that can be converted and a list of (input, output) pairs, 
-- represents a function that might return a string given a value
useTable :: (Eq a) => (a -> Bool) -> [(a, String)] -> SayMaybe a
useTable predicate table n 
  | predicate n = snd <$> find ((== n) . fst) table
  | otherwise = Nothing

-- printing function for "small" numbers in the range [0, 20[
saySmall :: (Integral a) => SayMaybe a
saySmall = 
  useTable (flip all [(>= 0), (<= 19)] . flip id) (zip [0..] numNames)
  where 
    numNames = words "zero one two three four five six seven eight nine ten \
      \ eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"

-- printing function for positive multiples of 10 <= 90
sayTens :: (Integral a) => SayMaybe a  
sayTens = 
  useTable (flip all [(>= 10), (<= 90), (== 0) . div 10] . flip id) (zip [10, 20..] numNames)
  where numNames = words "ten twenty thirty forty fifty sixty seventy eighty ninety"

-- printing function for all natural numbers less than 100
saySubHundreds :: (Integral a) => SayMaybe a
saySubHundreds n =
  case saySmall n of
    Just str -> Just str
    Nothing ->
      if smallPart == 0 then sayTens n
      else fmap unwords . sequence $ [sayTens (n - smallPart), saySmall smallPart]
  where smallPart = n `mod` 10 

-- printing function for all natural multiples of 100 >= 900
sayHundreds :: (Integral a) => SayMaybe a
sayHundreds n
  | all ($ n) [(>= 100), (<= 900), (== 0) . div 100] =
      Just $ (fromJust . saySmall) (n `div` 100) ++ " hundred"
  | n == 100 = Just "one hundred"
  | otherwise = Nothing

-- printing function for all natural numbers less than 1000
saySubThousands :: (Integral a) => SayMaybe a
saySubThousands n =
  case saySubHundreds n of
    Just str -> Just str
    Nothing -> 
      if smallPart == 0 then sayHundreds n
      else 
        fmap (unwords . intersperse "and") . sequence $ 
          [sayHundreds (n - smallPart), saySubHundreds smallPart]
  where
    smallPart = n `mod` 100

-- saysSubThousands composed with fromJust; uses assumption that supplied number
-- is in the range [0, 1000[ to return a String instead of a Maybe String
saySubThousands' :: (Integral a) => a -> String
saySubThousands' = fromJust . saySubThousands

powersOfTen :: [String]
powersOfTen = 
  ("" :) $
    words 
      "thousand million billion trillion quadrillion quintillion sextillion \
      \ septillion octillian nonillion decillion"

-- cuts number in a way such that
-- >> (sum $ zipWith (*) (map (10^) [0, 3..]) (numberParts n)) == n
-- and 
-- >> none (>= 1000) $ numberParts n
-- for any natural n
numberParts :: (Integral a) => a -> [a]
numberParts n
  | n < 1000 = [n]
  | otherwise = smallPart : numberParts ((n - smallPart) `div` 1000)
      where smallPart = n `mod` 1000

-- the top level export of this module; 
-- prints any number in the range ]r, r] where r = (10 ^ (3 * length powersOfTen))
-- (with the current implementation of powersOfTen, r = 10^36
-- when number is unprintable, "<very large number>" is supplied as result
sayNumber :: (Integral a) => a -> String
sayNumber n
  | not (numberPrintable n) = "<very large number>"
  | n < 0 = "negative " ++ (sayNumber . negate) n
  | otherwise = 
      intercalate  ", "  . reverse $
        map (\(i, p) -> if i /= 0 then saySubThousands' p ++ " " ++ powersOfTen !! i else saySubThousands' p) $ 
          filter ((/= 0) . snd) $ zip [0..] parts
      where
        parts = numberParts n

-- checks if number is in the established range; if it is, it is printable by sayNumber
numberPrintable :: (Integral a) => a -> Bool
numberPrintable n = abs n >= 10^(3 * length powersOfTen)
