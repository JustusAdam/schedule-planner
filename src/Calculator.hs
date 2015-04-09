module Calculator where

import           Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Ord    as Ord
import           Text.Printf



data Lesson = Lesson {
  number  :: Int,
  day     :: Int,
  weight  :: Int,
  subject :: String
} deriving (Show)


time :: Lesson -> (Int, Int)
time (Lesson {day=day, number=number}) = (day, number)


formatSchedule :: Map.Map (Int, Int) Lesson -> String
formatSchedule hours = intercalate "\n" $ [header] ++ (map formatDay allHours)
  where
    allHours = [(i, [1..7]) | i <- [1..7]]

    formatLesson :: (Int, Int) -> String
    formatLesson i =
      printf "%10v" (case (Map.lookup i hours) of
                        Nothing -> []
                        Just (Lesson {subject=subject}) -> subject
                      )

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = intercalate " | " [formatLesson (i, j) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight hours)


totalWeight :: Map.Map (Int, Int) Lesson -> Int
totalWeight m = Map.foldl (+) 0 (Map.map (\(Lesson {weight=weight}) -> weight) m)


calc :: [Lesson] -> [Map.Map (Int, Int) Lesson]
calc lessons =
  let
    mappedLessons = Map.fromListWith (++) (map (\x -> (subject x, [x])) lessons)
    sortedLessons = Map.map (List.sortBy (Ord.comparing weight)) mappedLessons
    (minListPrimer, listsValues) = unzip (map (\(t, (x : xs)) -> (x, (t, xs))) (Map.toList sortedLessons))
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
