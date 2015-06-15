module Main where

import SchedulePlanner.Scraper.TUDresden 
import System.IO 
import Data.Aeson 
import Data.ByteString.Lazy as L



main :: IO ()
main = do
  mdata <- scrapeTuDresden 4

  either print (print . encode) mdata
