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
import           Data.Either                       (rights)


universities :: [(String, Scraper)]
universities =
  [ ("tudresden", scrapeTuDresden) ]


toEither :: a -> Maybe b -> Either a b
toEither message = maybe (Left message) return


data ScraperOptions = ScraperOptions { semesters  :: [Int]
                                     , outputFile :: Maybe FilePath
                                     } deriving (Show)


scrape :: ScraperOptions -> String -> IO ()
scrape (ScraperOptions { semesters = semesters, outputFile = outputFile }) scraperName =
  handleScraperInput
  where
    scrapeAction scraper =
      putStrLn ("Trying to scrape semester " ++ show semesters ++ " for " ++ scraperName) >>
      serialize <$> scraper semesters >>= doIO
    handleScraperInput =
      maybe
        (putStrLn "This university is not supported (yet).")
        scrapeAction
        (lookup scraperName universities)
    serialize     = either fromString (encode . rights)
    writeConsole  = B.putStr . (`B.append` "\n")
    writeFile f   = withFile f WriteMode . flip B.hPutStr
    doIO          = maybe writeConsole writeFile outputFile
