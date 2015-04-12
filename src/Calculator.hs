{-|
Module      : $Header$
Description : Calculate schedules
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

This module provides functions for calculating possibilities for an ideal
schedule layout from weighted Lessons as well as providing functions for
converting them into readable/printable format.
-}
module Calculator (
  calc,
  formatSchedule,
  totalWeight,
  time,
  Lesson (..),
  Timeslot (..)
  ) where

import           Data.List     as List (intercalate, sortBy)
import qualified Data.Map.Lazy as Map (Map, empty, foldl, fromList,
                                       fromListWith, insert, lookup, map,
                                       toList)
import qualified Data.Ord      as Ord (comparing)
import           Text.Printf   (printf)


daysPerWeek = 7
slotsPerDay = 7


data Lesson = Lesson {
  timeslot :: Int,
  day      :: Int,
  weight   :: Int,
  subject  :: String
} deriving (Show)


-- |type Alias for readability
-- maps lessons to their respective subject
type MappedLessons  = Map.Map String [Lesson]
-- |type Alias for readability
-- (Slot, Day)
type Timeslot       = (Int, Int)
-- |type Alias for readability
-- represents a schedule
type MappedSchedule = Map.Map Timeslot Lesson



time :: Lesson -> Timeslot
time (Lesson {day=day, timeslot=timeslot}) = (day, timeslot)


{-
  Transform a MappedSchedule into a printable,
  and more importantly, readable String
-}
formatSchedule :: MappedSchedule -> String
formatSchedule hours =
  intercalate "\n" $ [header] ++ (map formatDay allHours)

  where
    allHours = [(i, [1..slotsPerDay]) | i <- [1..daysPerWeek]]

    formatLesson :: Timeslot -> String
    formatLesson i =
      printf "%10v" $ maybe [] subject (Map.lookup i hours)

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = intercalate " | " [formatLesson (i, j) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight hours)


totalWeight :: MappedSchedule -> Int
totalWeight m = Map.foldl (+) 0 $ Map.map weight m


{-
  Main evaluation function
  Transforms a list of weighted 'Lesson's into a list of lightest schedules
  by branching the evaluation at avery point where there is a timeslot collision
-}
calc :: [Lesson] -> [MappedSchedule]
calc lessons =
  calcStep x lists (Map.empty) minList
  where
    mappedLessons                 = Map.fromListWith (++) $ map (\x -> (subject x, [x])) lessons
    sortedLessons                 = Map.map (List.sortBy (Ord.comparing weight)) mappedLessons
    (minListPrimer, listsValues)  = unzip $ map (\(t, (x : xs)) -> (x, (t, xs))) $ Map.toList sortedLessons
    lists                         = Map.fromList listsValues
    (x : minList)                 = List.sortBy (Ord.comparing weight) minListPrimer


{-
  Helper function for calc
  represents a recusively called and forking calculation step
-}
calcStep :: Lesson -> MappedLessons -> MappedSchedule -> [Lesson] -> [MappedSchedule]
calcStep x lists hourMap minList =
  case (Map.lookup (time x) hourMap) of

    Nothing   ->
      if (null minList)
        then
          [newMap]
        else
          let (c : cs) = minList in
            calcStep c lists newMap cs

    Just old  ->
      let
        r1 = (reduceLists (subject x) lists) hourMap minList
        r2 = (reduceLists (subject old) lists) newMap minList
      in
      r1 ++ r2

  where
    newMap = Map.insert (time x) x hourMap

    reduceLists :: String -> MappedLessons -> (MappedSchedule -> [Lesson] -> [MappedSchedule])
    reduceLists s lists =
      case (Map.lookup s lists) of
        Nothing       -> noResult
        Just []       -> noResult
        Just (c : cs) -> calcStep c (Map.insert s cs lists)
        where
          noResult = \y x -> []
