{-# LANGUAGE OverloadedStrings #-}
module SchedulePlanner.Scraper
  ( scrapeTuDresden
  , scrape
  , ScraperOptions(..)
  ) where


import           Data.Aeson                        (encode)
import qualified Data.ByteString.Lazy              as B (append, hPutStr,
                                                         putStr)
import           Data.String                       (fromString)
import           SchedulePlanner.Scraper.Base      (Scraper)
import           SchedulePlanner.Scraper.TUDresden (scrapeTuDresden)
import           SchedulePlanner.Serialize         ()
import           System.IO                         (IOMode (WriteMode),
                                                    withFile)


universities :: [(String, Scraper)]
universities =
  [ ("tudresden", scrapeTuDresden) ]


toEither :: a -> Maybe b -> Either a b
toEither message = maybe (Left message) return


data ScraperOptions = ScraperOptions { semester   :: Maybe Int
                                     , outputFile :: Maybe FilePath
                                     } deriving (Show)


scrape :: ScraperOptions -> String -> IO ()
scrape (ScraperOptions { semester = semester, outputFile = outputFile }) scraperName =
  handleScraperInput
  where
    scrapeAction scraper semester =
      putStrLn ("Trying to scrape semester " ++ show semester ++ " for " ++ scraperName) >>
      serialize <$> scraper semester >>= doIO
    handleScraperInput = 
      maybe 
        (putStrLn "This university is not supported (yet).") 
        handleSemesterInput
        (lookup scraperName universities)
    handleSemesterInput scraper =
      maybe
        (putStrLn "Please provide a semester.")
        (scrapeAction scraper)
        semester
    serialize     = either fromString encode
    writeConsole  = B.putStr . (`B.append` "\n")
    writeFile f   = withFile f WriteMode . flip B.hPutStr
    doIO          = maybe writeConsole writeFile outputFile

