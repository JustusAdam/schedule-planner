{-# LANGUAGE UnicodeSyntax #-}
module SchedulePlanner.Scraper.Base where


import           Data.Text
import           SchedulePlanner.Types


newtype Semester = Semester { unSemester ∷ (Int, [Lesson Text]) }

type Scraper = ([Int] → IO (Either String [Either String Semester]))
