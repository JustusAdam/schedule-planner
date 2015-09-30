{-# LANGUAGE UnicodeSyntax #-}
module SchedulePlanner.Scraper.Base where


import           Data.Text
import           SchedulePlanner.Types
import Data.Map


newtype Semester = Semester { unSemester ∷ [Lesson Text] }

type Scraper = ([Int] → IO (Map Int (Either String Semester)))
