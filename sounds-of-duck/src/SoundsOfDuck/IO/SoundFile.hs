module SoundsOfDuck.IO.SoundFile
  ( ExportSettings(..)
  , defaultWavSettings
  , writeSnd 
  , renderSound ) where

import SoundsOfDuck.Sound
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
