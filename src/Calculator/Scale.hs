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
import           Data.Data
import           Data.List         as List
import qualified Data.Map          as Map
import           Data.Typeable


-- |The scope and target a 'Rule' whishes to influence
data Target         = Slot Int | Day Int | Cell Int Int deriving (Show, Typeable, Data)
-- |Weight increase by 'severity' for all 'Lesson's in target
data Rule           = Rule {target :: Target, severity :: Int} deriving (Show, Typeable, Data)
-- |Dynamic rule with only one condition
data SimpleDynRule  = SimpleDynRule {sDynTarget :: Target, sDynSeverity :: Int} deriving (Show)


-- |Type alias for more expressive function signature
type DayWeightMap   = Map.Map Int Int
-- |Type alias for more expressive function signature
type SlotWeightMap  = Map.Map Int Int
-- |Type alias for more expressive function signature
type CellWeighMap   = Map.Map (Int, Int) Int
-- |Type alias for more expressive function signature
type WeightMapTuple = (SlotWeightMap, DayWeightMap, CellWeighMap)
-- |Type alias for structure holding the dynamic rules
type DynRuleTuple   = (Map.Map Int [SimpleDynRule],
                       Map.Map Int [SimpleDynRule],
                       Map.Map (Int, Int) [SimpleDynRule])


-- |Scaffolding of a dynamic rule
class DynamicRule a where
  trigger          :: Lesson -> WeightMapTuple -> a -> (WeightMapTuple, a)
  getTriggerTarget :: a -> [Target]


instance DynamicRule SimpleDynRule where
  trigger inserted (ms, md, mc) (SimpleDynRule target sev) =
    case target of
      Slot slot   ->  ((ms, Map.insertWith (+) slot sev md, mc), rule)
      Day  day    ->  ((Map.insertWith (+) day sev ms, md, mc), rule)
      Cell c1 c2  ->  ((ms, md, Map.insertWith (+) (c1, c2) sev mc), rule)
    where
      rule = SimpleDynRule target sev

  getTriggerTarget (SimpleDynRule {sDynTarget = x}) = [x]


-- |Recalculate the lesson weight tuple as a result of dynamic rules
reCalcMaps :: WeightMapTuple -> Lesson -> DynRuleTuple -> (DynRuleTuple, WeightMapTuple)
reCalcMaps wmt l (rs, rd, rc) =
  ((rs, rd, rc), fst $ reCalcMaps' l (fst $ reCalcMaps' l (fst $ reCalcMaps' l wmt ls) ld) lc)
  where
    ls = Map.findWithDefault [] (timeslot l) rs
    ld = Map.findWithDefault [] (day l) rd
    lc = Map.findWithDefault [] (day l, timeslot l) rc


-- |Helper function for reCalcMaps
reCalcMaps' :: DynamicRule a => Lesson ->  WeightMapTuple -> [a] -> (WeightMapTuple, [a])
reCalcMaps' inserted = mapAccumL (trigger inserted)


-- |Apply a function to only the first element of a 2-tuple
applyToFirst :: (a -> c) -> (a, b) -> (c, b)
applyToFirst f (x, y) = (f x, y)


-- |Apply a function to only the second element of a 2-tuple
applyToSecond :: (b -> c) -> (a, b) -> (a, c)
applyToSecond f (x, y) = (x, f y)


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
