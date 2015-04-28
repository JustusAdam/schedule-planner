{-# LANGUAGE DeriveDataTypeable #-}
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
module Calculator.Solver (
  calcFromMap,
  calcFromList,
  mapToSubject,
  formatSchedule,
  totalWeight,
  time,
  Lesson (..),
  Timeslot (..),
  MappedSchedule,
  MappedLessons
  ) where

import           Data.List   as List (intercalate, sortBy, take)
import qualified Data.Map    as Map (Map, empty, foldl, fromList, fromListWith,
                                     insert, lookup, map, null, toList)
import qualified Data.Ord    as Ord (comparing)
import           Text.Printf (printf)
import Data.Typeable
import Data.Data


-- |How many days a week has
daysPerWeek = 7
-- |The amount of imeslots each day
slotsPerDay = 7
-- |The caracter width of a single slot in output
cellWidth   = 20


-- |Base datastructure for representing lessons
data Lesson = Lesson {
  timeslot :: Int,
  day      :: Int,
  weight   :: Int,
  subject  :: String
} deriving (Show, Eq, Ord, Typeable, Data)


-- |type Alias for readability
-- maps lessons to their respective subject
type MappedLessons  = Map.Map String [Lesson]
-- |type Alias for readability
-- (Slot, Day)
type Timeslot       = (Int, Int)
-- |type Alias for readability
-- represents a schedule
type MappedSchedule = Map.Map Timeslot Lesson


-- |Convenience function extracing the (day, timeslot) 'Tuple' from a 'Lesson'
time :: Lesson -> Timeslot
time (Lesson {day=day, timeslot=timeslot}) = (day, timeslot)


{-|
  Transform a 'MappedSchedule' into a printable,
  and more importantly, readable String
-}
formatSchedule :: MappedSchedule -> String
formatSchedule hours =
  intercalate "\n" $ header : map formatDay allHours

  where
    allHours = [(i, [1..slotsPerDay]) | i <- [1..daysPerWeek]]

    formatLesson :: Timeslot -> String
    formatLesson i =
      printf ("%" ++ show cellWidth ++"v") $ maybe [] (take cellWidth.subject) (Map.lookup i hours)

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = intercalate " | " [formatLesson (j, i) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight hours)


-- |Convenience function to obtain the total weight of a particular Schedule
totalWeight :: MappedSchedule -> Int
totalWeight m = Map.foldl (+) 0 $ Map.map weight m


{-
  Map a List of 'Lesson's to their respective subjects
-}
mapToSubject :: [Lesson] -> Map.Map String [Lesson]
mapToSubject [] = Map.empty
mapToSubject lessons = Map.fromListWith (++) $ map (\x -> (subject x, [x])) lessons


{-|
  Same as 'calcFromMap' but operates on a List of 'Lesson's
-}
calcFromList :: [Lesson] -> [MappedSchedule]
calcFromList = calcFromMap.mapToSubject



{-
  Main evaluation function
  Transforms a map of weighted 'Lesson's of a particular subject into a list
  of lightest schedules by branching the evaluation at avery point
  where there is a timeslot collision
-}
calcFromMap :: Map.Map String [Lesson] -> [MappedSchedule]
calcFromMap mappedLessons
  | Map.null mappedLessons = []
  | otherwise = calc' x lists Map.empty minList
  where
    sortedLessons                 = Map.map (List.sortBy (Ord.comparing weight)) mappedLessons
    (minListPrimer, listsValues)  = unzip $ map (\(t, x : xs) -> (x, (t, xs))) $ Map.toList sortedLessons
    lists                         = Map.fromList listsValues
    (x : minList)                 = List.sortBy (Ord.comparing weight) minListPrimer


{-|
  Helper function for 'calc'
  represents a recusively called and forking calculation step
-}
calc' :: Lesson -> MappedLessons -> MappedSchedule -> [Lesson] -> [MappedSchedule]
calc' x lists hourMap minList =
  case Map.lookup (time x) hourMap of

    Nothing   ->
      if null minList
        then
          [newMap]
        else
          let (c : cs) = minList in
            calc' c lists newMap cs

    Just old  ->
      let
        r1 = reduceLists (subject x) lists hourMap minList
        r2 = reduceLists (subject old) lists newMap minList
      in
      r1 ++ r2

  where
    newMap = Map.insert (time x) x hourMap

    reduceLists :: String -> MappedLessons -> MappedSchedule -> [Lesson] -> [MappedSchedule]
    reduceLists s lists =
      case Map.lookup s lists of
        Nothing       -> noResult
        Just []       -> noResult
        Just (c : cs) -> calc' c (Map.insert s cs lists)
        where
          noResult y x = []
