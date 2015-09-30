{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module SchedulePlanner.Scraper
  ( scrapeTuDresden
  , scrape
  , ScraperOptions(..)
  ) where


import           Data.Aeson
import qualified Data.ByteString.Lazy              as B (hPutStr, putStr)
import           Data.Char                         (toLower)
import qualified Data.Map                          as Map
import           Data.Monoid.Unicode               ((⊕))
import           Prelude.Unicode
import           SchedulePlanner.Scraper.Base      (Scraper)
import           SchedulePlanner.Scraper.TUDresden (scrapeTuDresden)
import           SchedulePlanner.Serialize         ()
import           System.IO                         (IOMode (WriteMode),
                                                    withFile)


universities ∷ [(String, Scraper)]
universities =
  [ ("tudresden", scrapeTuDresden) ]


data ScraperOptions = ScraperOptions { semesters  ∷ [Int]
                                     , outputFile ∷ Maybe FilePath
                                     } deriving (Show)


scrape ∷ ScraperOptions → String → IO ()
scrape (ScraperOptions { semesters, outputFile }) scraperName =
  handleScraperInput
  where
    scraperName' = fmap toLower scraperName
    scrapeAction scraper = do
      putStrLn ("Trying to scrape semester " ⊕ show semesters ⊕ " for " ⊕ scraperName)
      data' ← serialize <$> scraper semesters
      doIO data'
    handleScraperInput =
      maybe
        (putStrLn "This university is not supported (yet).")
        scrapeAction
        (lookup scraperName' universities)
    serialize     = encode ∘ fmap (object ∘ sequence [("semester" .=) . fst, ("lessons" .=) . snd]) ∘ Map.toList
    writeConsole  = B.putStr ∘ (⊕ "\n")
    write f       = withFile f WriteMode ∘ flip B.hPutStr
    doIO          = maybe writeConsole write outputFile
