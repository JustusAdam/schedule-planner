{-# LANGUAGE ConstraintKinds, ExplicitForAll, ScopedTypeVariables #-}
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
  , MappedSchedule(..)
  , MappedLessons(..)
  ) where

import           Control.Arrow         ((&&&))
import           Data.Maybe            (fromMaybe)
import qualified Data.Ord              as Ord (comparing)
import ClassyPrelude
import           SchedulePlanner.Types
import Data.Map (fromListWith)



{-|
  type Alias for readability
  maps lessons to their respective subject
-}
type IsMappedLessons s map  = (IsMap map, ContainerKey map ~ s, MapValue map ~ [(Lesson s, Weight)])


{-|
  type Alias for readability
  represents a schedule
-}
type IsMappedSchedule s map = (IsMap map, ContainerKey map ~ Cell, MapValue map ~ (Lesson s, Weight))


-- | Convenience function extracing the (day, timeslot) 'Tuple' from a 'Lesson'
time :: Lesson a -> Cell
time = Cell . (day &&& timeslot)


-- | Convenience function to obtain the total weight of a particular Schedule
totalWeight :: IsMappedSchedule a map => map -> Integer
totalWeight = sum . map (snd . snd) . mapToList


{-|
  Map a List of 'Lesson's to their respective subjects
-}
mapToSubject :: Ord w => [(Lesson w, Weight)] -> Map w [(Lesson w, Weight)]
mapToSubject = fromListWith (++) . map ((subject . fst) &&& return)


{-|
  Same as 'calcFromMap' but operates on a List of 'Lesson's
-}
calcFromList :: (IsMappedSchedule w map, Ord w) => [(Lesson w, Weight)] -> Maybe [map]
calcFromList = calcFromMap . mapToSubject


{-|
  Main evaluation function
  Transforms a map of weighted 'Lesson's of a particular subject into a list
  of lightest schedules by branching the evaluation at avery point
  where there is a timeslot collision
-}
calcFromMap :: (IsMappedSchedule w smap, IsMappedLessons w lmap, Ord w, Monoid smap)
            => lmap
            -> [smap]
calcFromMap (null -> True) = []
calcFromMap mappedLessons = reduceLists subjX sortedLessons mempty minList
  where
    sortedLessons     = map (sortBy (Ord.comparing snd)) mappedLessons
    (subjX : minList) = keys sortedLessons


{-|
  One of the essential calculation steps, reducing the subject lists and
  recursing the calculation
-}
reduceLists :: (IsMappedSchedule w smap, IsMappedLessons w lmap, Ord w)
            => w
            -> lmap
            -> smap
            -> [w]
            -> [smap]
reduceLists s mappedLessons schedules subjects =
  fromMaybe [] $ lookup s mappedLessons >>= uncons >>=
    \(c, cs)  -> calc' c (insertMap s cs mappedLessons) schedules subjects


{-|
  Helper function for 'calcFromMap'
  represents a recusively called and forking calculation step
-}
calc' :: (IsMappedSchedule w smap, IsMappedLessons w lmap, Ord w)
      => Lesson w
      -> lmap
      -> smap
      -> [ɷ]
      -> [smap]
calc' x lists hourMap minList =
  maybe
    maybeEnd
    splitCalc
    (lookup (time x) hourMap)

  where
    maybeEnd = maybe
                [newMap]
                (\(c, cs) -> reduceLists c lists newMap cs)
                (uncons minList)

    sideCalc element aMap = fromMaybe [] (reduceLists (subject element) lists aMap minList)

    splitCalc old         = sideCalc x hourMap ++ sideCalc old newMap

    newMap                = insertMap (time x) x hourMap
