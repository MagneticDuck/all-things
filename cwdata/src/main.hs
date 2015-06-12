module Main where

import CWData.Data
import Control.Applicative
-- import Control.Monad

main :: IO ()
main = 
  print =<<getDownloads
    


getDownloads :: IO Int
getDownloads = sum . map _mapDownloads <$> getMaps


