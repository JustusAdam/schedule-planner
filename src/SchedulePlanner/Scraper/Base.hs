module SchedulePlanner.Scraper.Base where


import SchedulePlanner.Types
import Data.Text


newtype Semester = Semester { unSemester :: (Int, [Lesson Text]) }

type Scraper = ([Int] -> IO (Either String [Either String Semester]))
