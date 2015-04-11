module Scale where

import           Calculator
import           Data.List     as List
import qualified Data.Map.Lazy as Map


data Target = Slot Int | Day Int | Cell Int Int
data Rule   = Rule {target::Target, severity::Int}


type DayWeightMap   = Map.Map Int Int
type SlotWeightMap  = Map.Map Int Int
type CellWeighMap   = Map.Map (Int, Int) Int
type WeightMapTuple = (SlotWeightMap, DayWeightMap, CellWeighMap)


weigh :: [Rule] -> [Lesson] -> [Lesson]
weigh [] x  = x
weigh _ []  = []
weigh rs ls = do
  l <- ls
  return (weighOne maps l)

  where
    maps = calcMaps rs


weighOne :: WeightMapTuple -> Lesson -> Lesson
weighOne (ms, md, mc) l =
  l {
      weight = oldWeight + slotWeight + dayWeight + cellWeight
    }

  where
    getOrZero :: Ord k => k -> Map.Map k Int -> Int
    getOrZero  = Map.findWithDefault 0

    oldWeight  = weight l
    dayWeight  = getOrZero (day l) md
    slotWeight = getOrZero (timeslot l) ms
    cellWeight = getOrZero (time l) mc


calcMaps :: [Rule] -> WeightMapTuple
calcMaps r
  | (List.null r) = allEmpty
  | otherwise     = calcMapsStep r allEmpty
  where
    allEmpty = (Map.empty, Map.empty, Map.empty)

calcMapsStep :: [Rule] -> WeightMapTuple -> WeightMapTuple
calcMapsStep [] mt                          = mt
calcMapsStep ((Rule t sev):xs) (ms, md, mc) = calcMapsStep xs newMaps

  where
    increase :: Ord k => k -> Int -> Map.Map k Int -> Map.Map k Int
    increase = Map.insertWith (+)

    newMaps = case t of
                Slot s      -> ((increase s sev ms), md, mc)
                Day d       -> (ms, (increase d sev md), mc)
                Cell c1 c2  -> (ms, md, (increase (c1, c2) sev mc))
