module SchedulePlanner.Scraper.Base where


import           SchedulePlanner.Types
import ClassyPrelude


newtype Semester = Semester { unSemester :: [Lesson Text] }

type Scraper = ([Int] -> IO (Map Int (Either String Semester)))
