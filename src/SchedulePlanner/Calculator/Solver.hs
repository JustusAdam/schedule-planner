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
module SchedulePlanner.Calculator.Solver
  ( calcFromMap
  , calcFromList
  , mapToSubject
  , totalWeight
  , time
  , Lesson (..)
  , Timeslot
  , MappedSchedule
  , MappedLessons
  ) where

import           Data.Data     (Data)
import           Data.List     as List (sortBy, uncons)
import qualified Data.Map      as Map (Map, empty, foldl, fromListWith, insert,
                                       keys, lookup, map, null)
import           Data.Maybe    (fromMaybe)
import qualified Data.Ord      as Ord (comparing)
import           Data.Typeable (Typeable)


-- | Base datastructure for representing lessons
data Lesson s = Lesson {
  timeslot :: Int,
  day      :: Int,
  weight   :: Int,
  subject  :: s
} deriving (Show, Eq, Ord, Typeable, Data)


{-|
  type Alias for readability
  maps lessons to their respective subject
-}
type MappedLessons s  = Map.Map s [Lesson s]


{-|
  type Alias for readability
  (Slot, Day)
-}
type Timeslot         = (Int, Int)


{-|
  type Alias for readability
  represents a schedule
-}
type MappedSchedule s = Map.Map Timeslot (Lesson s)


-- | Convenience function extracing the (day, timeslot) 'Tuple' from a 'Lesson'
time :: Lesson a -> Timeslot
time = pure (,) <*> day <*> timeslot


-- | Convenience function to obtain the total weight of a particular Schedule
totalWeight :: MappedSchedule a -> Int
totalWeight = Map.foldl (+) 0 . Map.map weight


{-|
  Map a List of 'Lesson's to their respective subjects
-}
mapToSubject :: Ord s => [Lesson s] -> Map.Map s [Lesson s]
mapToSubject = Map.fromListWith (++) . map (pure (,) <*> subject <*> (:[]))


{-|
  Same as 'calcFromMap' but operates on a List of 'Lesson's
-}
calcFromList :: Ord s => [Lesson s] -> Maybe [MappedSchedule s]
calcFromList = calcFromMap.mapToSubject


{-|
  Main evaluation function
  Transforms a map of weighted 'Lesson's of a particular subject into a list
  of lightest schedules by branching the evaluation at avery point
  where there is a timeslot collision
-}
calcFromMap :: Ord s
            => Map.Map s [Lesson s]
            -> Maybe [MappedSchedule s]
calcFromMap mappedLessons
  | Map.null mappedLessons  = Nothing
  | otherwise               = reduceLists subjX sortedLessons Map.empty minList
  where
    sortedLessons       = Map.map (List.sortBy (Ord.comparing weight)) mappedLessons
    (subjX : minList)   = Map.keys sortedLessons


{-|
  One of the essential calculation steps, reducing the subject lists and
  recursing the calculation
-}
reduceLists :: Ord s
            => s
            -> MappedLessons s
            -> MappedSchedule s
            -> [s]
            -> Maybe [MappedSchedule s]
reduceLists s mappedLessons schedules subjects =
  Map.lookup s mappedLessons >>= uncons >>=
    \(c, cs)  -> calc' c (Map.insert s cs mappedLessons) schedules subjects


{-|
  Helper function for 'calcFromMap'
  represents a recusively called and forking calculation step
-}
calc' :: Ord s => Lesson s -> MappedLessons s -> MappedSchedule s -> [s] -> Maybe [MappedSchedule s]
calc' x lists hourMap minList =
  maybe
    maybeEnd
    splitCalc
    (Map.lookup (time x) hourMap)

  where
    maybeEnd = maybe
                (return [newMap])
                (\(c, cs) -> reduceLists c lists newMap cs)
                (uncons minList)

    sideCalc element aMap = fromMaybe [] (reduceLists (subject element) lists aMap minList)

    splitCalc old = return $ sideCalc x hourMap ++ sideCalc old newMap

    newMap = Map.insert (time x) x hourMap
