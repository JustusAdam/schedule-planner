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

import           Data.Data
import           Data.List     as List (intercalate, sortBy, take)
import qualified Data.Map      as Map (Map, empty, foldl, fromList,
                                       fromListWith, insert, keys, lookup, map,
                                       null, toList)
import           Data.Maybe    (fromMaybe)
import qualified Data.Ord      as Ord (comparing)
import           Data.Typeable
import           Text.Printf   (printf)


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
-- time (Lesson {day=day, timeslot=timeslot}) = (day, timeslot)
time = pure (,) <*> day <*> timeslot


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
      printf ("%" ++ show cellWidth ++ "v") $ maybe [] (reverse.take cellWidth.reverse.subject) (Map.lookup i hours)

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = intercalate " | " [formatLesson (j, i) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight hours)


-- |Convenience function to obtain the total weight of a particular Schedule
totalWeight :: MappedSchedule -> Int
totalWeight = (Map.foldl (+) 0).(Map.map weight)


{-|
  Map a List of 'Lesson's to their respective subjects
-}
mapToSubject :: [Lesson] -> Map.Map String [Lesson]
mapToSubject = (Map.fromListWith (++)).(map (\x -> (subject x, [x])))


{-|
  Same as 'calcFromMap' but operates on a List of 'Lesson's
-}
calcFromList :: [Lesson] -> Maybe [MappedSchedule]
calcFromList = calcFromMap.mapToSubject



{-|
  Main evaluation function
  Transforms a map of weighted 'Lesson's of a particular subject into a list
  of lightest schedules by branching the evaluation at avery point
  where there is a timeslot collision
-}
calcFromMap :: Map.Map String [Lesson] -> Maybe [MappedSchedule]
calcFromMap mappedLessons
  | Map.null mappedLessons  = Nothing
  | otherwise               = Map.lookup subjX sortedLessons >>=
    (\(x : xs) ->
      calc' x (Map.insert subjX xs sortedLessons) Map.empty minList
      )
  where
    sortedLessons       = Map.map (List.sortBy (Ord.comparing weight)) mappedLessons
    (subjX : minList)   = Map.keys sortedLessons


{-|
  Helper function for 'calcFromMap'
  represents a recusively called and forking calculation step
-}
calc' :: Lesson -> MappedLessons -> MappedSchedule -> [String] -> Maybe [MappedSchedule]
calc' x lists hourMap minList =
  case Map.lookup (time x) hourMap of

    Nothing   ->
      case minList of
        []        -> return [newMap]
        (c : cs)  -> Map.lookup c lists >>=
          (\(l : ls) -> calc' l (Map.insert c ls lists) newMap cs)

    Just old  ->
      return $ r1 ++ r2
      where
        r1 = fromMaybe [] $ reduceLists (subject x)   lists hourMap minList
        r2 = fromMaybe [] $ reduceLists (subject old) lists newMap  minList

  where
    newMap = Map.insert (time x) x hourMap

    reduceLists :: String -> MappedLessons -> MappedSchedule -> [String] -> Maybe [MappedSchedule]
    reduceLists s lists schedules subjects = Map.lookup s lists >>=
      (\l ->
        case l of
          []        -> Nothing
          (c : cs)  -> calc' c (Map.insert s cs lists) schedules subjects
        )
