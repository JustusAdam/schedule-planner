module SchedulePlanner.Scraper.Base where


import SchedulePlanner.Types
import Data.Text


type Scraper = (Int -> IO (Either String [Lesson Text]))
