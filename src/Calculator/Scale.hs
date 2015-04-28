{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
Description : Apply weighing rules to lessons
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

This module is used to weigh a list of lessons according to rules.
-}
module Calculator.Scale (
  weigh,
  Rule(..),
  Target(..),
  calcMaps
) where

import           Calculator.Solver
import           Data.List         as List
import qualified Data.Map          as Map
import Data.Typeable
import Data.Data


-- |The scope and target a 'Rule' whishes to influence
data Target   = Slot Int | Day Int | Cell Int Int deriving (Show, Typeable, Data)
-- |Weight increase by 'severity' for all 'Lesson's in target
data Rule     = Rule {target::Target, severity::Int} deriving (Show, Typeable, Data)

data DayRule  = DayRule {dayRuleDay :: Int, dayRuleSeverity::Int}

data SlotRule = SlotRule {slotRuleSlot :: Int, slotRuleSeverity :: Int}

data CellRule = CellRule {cellRuleDay :: Int, cellRuleSlot :: Int, cellRuleSeverity :: Int}


-- |type alias for more expressive function signature
type DayWeightMap   = Map.Map Int Int
-- |type alias for more expressive function signature
type SlotWeightMap  = Map.Map Int Int
-- |type alias for more expressive function signature
type CellWeighMap   = Map.Map (Int, Int) Int
-- |type alias for more expressive function signature
type WeightMapTuple = (SlotWeightMap, DayWeightMap, CellWeighMap)

type DynRuleTuple   = (Map.Map Int [DayRule], Map.Map Int [SlotRule], Map.Map (Int, Int) [CellRule])


class DynamicRule a where
    trigger          :: Lesson -> WeightMapTuple -> a -> (WeightMapTuple, a)
    getTriggerTarget :: a -> [Target]

instance DynamicRule DayRule where
    trigger inserted (ms, md, mc) rule =
        ((ms, newMd, mc), rule)
        where
            newMd = Map.insertWith (+) (dayRuleDay rule) (dayRuleSeverity rule) md
    getTriggerTarget rule = [Day (dayRuleDay rule)]

instance DynamicRule SlotRule where
    trigger inserted (ms, md, mc) rule =
        ((newMs, md, mc), rule)
        where
            newMs = Map.insertWith (+) (slotRuleSlot rule) (slotRuleSeverity rule) ms
    getTriggerTarget rule = [Slot (slotRuleSlot rule)]

instance DynamicRule CellRule where
    trigger inserted (ms, md, mc) rule =
        ((ms, md, newMc), rule)
        where
            newMc = Map.insertWith (+) (cellRuleDay rule, cellRuleSlot rule) (cellRuleSeverity rule) mc
    getTriggerTarget rule = [Cell (cellRuleDay rule) (cellRuleSlot rule)]


reweight :: WeightMapTuple -> Lesson -> DynRuleTuple -> (DynRuleTuple, WeightMapTuple)
reweight wmt l (rs, rd, rc) =
    ((rs, rd, rc), fst $ reweight' l (fst $ reweight' l (fst $ reweight' l wmt ls) ld) lc)
    where
        ls = Map.findWithDefault [] (timeslot l) rs
        ld = Map.findWithDefault [] (day l) rd
        lc = Map.findWithDefault [] (day l, timeslot l) rc


applyToFirst :: (a -> c) -> (a, b) -> (c, b)
applyToFirst f (x, y) = (f x, y)


applyToSecond :: (b -> c) -> (a, b) -> (a, c)
applyToSecond f (x, y) = (x, f y)


reweight' :: DynamicRule a => Lesson ->  WeightMapTuple -> [a] -> (WeightMapTuple, [a])
reweight' inserted = mapAccumL (trigger inserted)


{-|
  Main function of the module.

  This funcion calculates weights the 'Lesson's provided applying the
  'Rule's provided.

  Resulting 'Lesson's are exactly the same, except for the weight
  component which is the old weight + the weight calculated from the rules
-}
weigh :: [Rule] -> [Lesson] -> [Lesson]
weigh [] x  = x
weigh _ []  = []
weigh rs ls = do
  l <- ls
  return (weighOne maps l)

  where
    maps = calcMaps rs


{-|
  Weighs a single 'Lesson', but instead of 'Rule's expects a
  'Tuple' of weight increase maps.
-}
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


{-|
  Contruct a 'Tuple' of /scope -> weight increase/ maps for more efficient
  weighing afterwards
-}
calcMaps :: [Rule] -> WeightMapTuple
calcMaps r
  | List.null r   = allEmpty
  | otherwise     = calcMapsStep r allEmpty

  where
    allEmpty = (Map.empty, Map.empty, Map.empty)


{-|
  Recursive step for the actual calculation done by 'calcMaps'
-}
calcMapsStep :: [Rule] -> WeightMapTuple -> WeightMapTuple
calcMapsStep [] mt                          = mt
calcMapsStep (Rule t sev :xs) (ms, md, mc)  = calcMapsStep xs newMaps

  where
    increase :: Ord k => k -> Int -> Map.Map k Int -> Map.Map k Int
    increase = Map.insertWith (+)

    newMaps = case t of
                Slot s      -> (increase s sev ms, md, mc)
                Day d       -> (ms, increase d sev md, mc)
                Cell c1 c2  -> (ms, md, increase (c1, c2) sev mc)
