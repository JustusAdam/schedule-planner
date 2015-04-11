module Calculator (
  calc,
  formatSchedule,
  totalWeight,
  time,
  Lesson (..),
  MappedLessons (..),
  MappedSchedule (..),
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


-- type aliases for readability
type MappedLessons = Map.Map String [Lesson]
type MappedSchedule = Map.Map (Int, Int) Lesson
type Timeslot = (Int, Int)


time :: Lesson -> (Int, Int)
time (Lesson {day=day, timeslot=timeslot}) = (day, timeslot)


-- Transform a MappedSchedule into a printable,
-- and more importantly, readable String
formatSchedule :: MappedSchedule -> String
formatSchedule hours = intercalate "\n" $ [header] ++ (map formatDay allHours)
  where
    allHours = [(i, [1..slotsPerDay]) | i <- [1..daysPerWeek]]

    formatLesson :: Timeslot -> String
    formatLesson i =
      printf "%10v" $ case (Map.lookup i hours) of
                        Nothing -> []
                        Just (Lesson {subject=subject}) -> subject

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = intercalate " | " [formatLesson (i, j) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight hours)


totalWeight :: MappedSchedule -> Int
totalWeight m = Map.foldl (+) 0 $ Map.map (\(Lesson {weight=weight}) -> weight) m


-- Main evaluation function
-- Transforms a list of weighted lessons into a list of lightest schedules
-- by branching the evaluation at avery point where there is timeslot collision

-- Afterwards one may obtain the lightest schedule by evaluating the `min` with
-- the `totalWeight` function
calc :: [Lesson] -> [MappedSchedule]
calc lessons =
  let
    mappedLessons = Map.fromListWith (++) $ map (\x -> (subject x, [x])) lessons
    sortedLessons = Map.map (List.sortBy (Ord.comparing weight)) mappedLessons
    (minListPrimer, listsValues) = unzip $ map (\(t, (x : xs)) -> (x, (t, xs))) $ Map.toList sortedLessons
    lists = Map.fromList listsValues
    (x : minList) = List.sortBy (Ord.comparing weight) minListPrimer
  in
    calcStep x lists (Map.empty) minList


-- Helper function for calc
-- represents a recusively called and forking calculation step
calcStep :: Lesson -> MappedLessons -> MappedSchedule -> [Lesson] -> [MappedSchedule]
calcStep x lists hourMap minList =
  case (Map.lookup (time x) hourMap) of
    Nothing ->
      if (null minList)
        then
          [newMap]
        else
          let (c : cs) = minList in
            calcStep c lists newMap cs
    Just old ->
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
        Nothing -> noResult
        Just [] -> noResult
        Just (c : cs) -> calcStep c (Map.insert s cs lists)
        where
          noResult = \y x -> []
