module SoundsOfDuck.Raw
  ( ExportSettings(..)
  , defaultWavSettings
  , writeSnd 
  , Duration
  , Sound(..) 
  , renderSound ) where

import qualified Sound.File.Sndfile as Snd
import Control.Applicative
import Foreign.Marshal.Array
import System.IO (hGetContents, Handle, openFile, IOMode(..))

-- these are the settings that the user supplies, 
-- in addition to the list of samples, to generate a sound file
data ExportSettings =
  ExportSettings 
    { getFormat :: Snd.Format -- the file format in question 
                              -- (header, encoding and endianess)
    , getFilePath :: FilePath -- the path where the file will be written to 
                              -- cannot be a directory
    , getSampleRate :: Int}   -- the number of samples per second

defaultWavFormat :: Snd.Format
defaultWavFormat = 
  Snd.Format Snd.HeaderFormatWav Snd.SampleFormatFloat Snd.EndianFile

defaultWavSettings :: ExportSettings
defaultWavSettings =
  ExportSettings
    { getFormat = defaultWavFormat
    , getFilePath = "output.wav"
    , getSampleRate = 41100 }

openSndHandle :: ExportSettings -> [Float] -> IO Snd.Handle
openSndHandle es frames =
    let 
      info = Snd.Info (length frames) (getSampleRate es) 1 (getFormat es) 1 True
    in Snd.openFile (getFilePath es) Snd.WriteMode info

type FloatSamples = [Float]

writeSnd :: ExportSettings -> FloatSamples -> IO ()
writeSnd es frames = (openSndHandle es frames) >>= \h ->
              newArray frames >>= \ptr ->
              Snd.hPutBuf h ptr (length frames) >>= \c ->
              return ()

-- now some slightly higher level sounds

type Duration = Float -- an amount of time in seconds

data Sound = 
  Sound (Duration -> Float) Duration
  -- this abstract "sound" type represents a sound with a sample generation
  -- function that takes a time in seconds and returns a sample values
  -- between 1 and -1 as first parameter, and two durations that 
  -- define the range of time in which the sound should be rendered
  -- the first duration is u

sampleMoments :: Int -> [Float]
sampleMoments samplerate =
  map ((/(fromIntegral samplerate)) . fromIntegral) [0, 1..]

clipSound :: [Float] -> [Float]
clipSound = map (\x -> if abs x > 1 then signum x else x)

soundToSamples :: Int -> Sound -> [Float]
soundToSamples sr (Sound f dur) =
  clipSound . (map f) . (takeWhile (< dur)) $ sampleMoments sr

renderSound :: ExportSettings -> Sound -> IO ()
renderSound es s =
  writeSnd es (soundToSamples (getSampleRate es) s)
