module SchedulePlanner.Scraper
  ( scrapeTuDresden
  , scrape
  , ScraperOptions(..)
  ) where


import SchedulePlanner.Scraper.TUDresden
import Data.ByteString.Lazy as B
import Data.Aeson
import System.IO
import Data.String
import SchedulePlanner.Serialize ()


data ScraperOptions = ScraperOptions { semester :: Maybe Int, outputFile :: Maybe FilePath } deriving (Show)


scrape :: ScraperOptions -> String -> IO ()
scrape (ScraperOptions { semester = semester, outputFile = outputFile }) _ =
  (either fromString encode <$> maybe (return $ Left "Please provide a semester to scrape for") scrapeTuDresden semester) >>=
      maybe B.putStr (\f -> withFile f WriteMode . flip B.hPutStr) outputFile
  
  
