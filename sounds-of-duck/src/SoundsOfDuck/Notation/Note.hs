module SoundsOfDuck.Notation.Note
  ( freqFromString
  , Frequency )
  where

import Data.Char

type Octave = Int
data Key =
  CKey
  | DKey
  | EKey
  | FKey 
  | GKey
  | AKey
  | BKey deriving (Show)
data Accidental =
  Sharp
  | FloatSharp
  | Flat
  | FloatFlat
  | Natural deriving (Show)
data Note = Note Octave Key Accidental deriving (Show)

maybeKeyFromString :: String -> Maybe Key
maybeKeyFromString str =
  if length str > 0 then
    case toUpper (head str) of
      'C' -> Just $ CKey
      'D' -> Just $ DKey
      'E' -> Just $ EKey
      'F' -> Just $ FKey 
      'G' -> Just $ GKey 
      'A' -> Just $ AKey
      'B' -> Just $ BKey
      _ -> Nothing
  else
    Nothing

maybeAccidentalFromString :: String -> Maybe Accidental
maybeAccidentalFromString str =
  case length str of
    0 -> Nothing
    1 ->
      case str of 
        "#" -> Just Sharp
        "b" -> Just Flat
        _ -> Nothing
    _ ->
      case (take 2 str) of
        "##" -> Just FloatSharp
        "bb" -> Just FloatFlat
        twostr ->
          case head twostr of
            '#' -> Just Sharp
            'b' -> Just Flat 

maybeOctaveFromString :: String -> Maybe Octave
maybeOctaveFromString str =
  if null str then
    Nothing
  else
    case last str of
      '0' -> Just 0
      '1' -> Just 1
      '2' -> Just 2
      '3' -> Just 3
      '4' -> Just 4
      '5' -> Just 5
      '6' -> Just 6
      '7' -> Just 7
      '8' -> Just 8
      _ -> Nothing

maybeNoteFromString :: String -> Maybe Note
maybeNoteFromString str = 
  case maybeKeyFromString str of
    Just anote ->
      let
        accidental = maybeAccidentalFromString $ tail str
        octave = maybeOctaveFromString str
      in
        case accidental of
          Just anAccidental ->
            case octave of
              Just anOctave ->
                Just $ Note anOctave anote anAccidental 
              Nothing -> Just $ Note 4 anote anAccidental
          Nothing ->
            case octave of
              Just anOctave ->
                Just $ Note anOctave anote Natural
              Nothing ->
                Just $ Note 4 anote Natural
    Nothing -> Nothing

freqFromIndex :: Int -> Float
freqFromIndex x =
  case x of
    1 -> 16.35
    2 -> 17.32
    3 -> 18.35
    4 -> 19.45
    5 -> 20.60
    6 -> 21.83
    7 -> 23.12
    8 -> 24.50
    9 -> 25.96
    10 -> 27.50
    11 -> 29.14
    12 -> 30.78

indexFromKeyAccidental :: Key -> Accidental -> Int
indexFromKeyAccidental key accidental =
  case key of
    CKey ->
      case accidental of
        FloatFlat  -> 11
        Flat        -> 12
        Natural     -> 1
        Sharp       -> 2
        FloatSharp -> 3
    DKey ->
      case accidental of
        FloatFlat  -> 1
        Flat        -> 2
        Natural     -> 3
        Sharp       -> 4
        FloatSharp -> 5
    EKey -> 
      case accidental of
        FloatFlat  -> 3
        Flat        -> 4
        Natural     -> 5
        Sharp       -> 6
        FloatSharp -> 7
    FKey ->
      case accidental of
        FloatFlat  -> 4
        Flat        -> 5
        Natural     -> 6
        Sharp       -> 7
        FloatSharp -> 8
    GKey ->
      case accidental of
        FloatFlat  -> 6
        Flat        -> 7
        Natural     -> 8
        Sharp       -> 9
        FloatSharp -> 10
    AKey ->
      case accidental of
        FloatFlat  -> 8
        Flat        -> 9
        Natural     -> 10
        Sharp       -> 11
        FloatSharp -> 12
    BKey ->
      case accidental of
        FloatFlat  -> 10
        Flat        -> 11
        Natural     -> 12
        Sharp       -> 1
        FloatSharp -> 2

freqFromNote :: Note -> Float
freqFromNote (Note octave key accidental) =
  let
    indexFreq = freqFromIndex $ indexFromKeyAccidental key accidental
  in
    indexFreq * (2 ^ octave)

freqFromString :: String -> Frequency
freqFromString str =
  case maybeNoteFromString str of
    Just anote -> freqFromNote anote
    Nothing -> freqFromString "c4"

type Frequency = Float
