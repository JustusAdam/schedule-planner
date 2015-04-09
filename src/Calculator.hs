module Calculator where

import qualified Data.Map as Map
import Data.List as List
import qualified Data.Ord as Ord



data Lesson = Lesson {
  number :: Int,
  day :: Int,
  weight :: Int,
  subject :: String
} deriving (Show)


time :: Lesson -> (Int, Int)
time (Lesson {day=day, number=number}) = (day, number)

calc :: [Lesson] -> [Map.Map (Int, Int) Lesson]
calc lessons =
  let
    mappedLessons = List.groupBy (\(Lesson {weight=x}) (Lesson {weight=y}) -> x == y) lessons
    sortedLessons = map (List.sortBy (Ord.comparing subject)) mappedLessons
    (minListPrimer, listsValues) = unzip (map (\(x : xs) -> (x, ((subject x), xs))) sortedLessons)
    lists = Map.fromList listsValues
    (x : minList) = List.sortBy (Ord.comparing weight) minListPrimer
  in
    calcStep x lists (Map.empty) minList



calcStep :: Lesson -> Map.Map String [Lesson] -> Map.Map (Int, Int) Lesson -> [Lesson] -> [Map.Map (Int, Int) Lesson]
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

reduceLists :: String -> Map.Map String [Lesson] -> (Map.Map (Int, Int) Lesson -> [Lesson] -> [Map.Map (Int, Int) Lesson])
reduceLists s lists =
  case (Map.lookup s lists) of
    Nothing -> noResult
    Just [] -> noResult
    Just (c : cs) -> calcStep c (Map.insert s cs lists)
    where
      noResult = \y x -> []
