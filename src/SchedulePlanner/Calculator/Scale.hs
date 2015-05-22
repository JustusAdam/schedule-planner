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
module SchedulePlanner.Calculator.Scale
  ( weigh
  , Rule(..)
  , Target(..)
  , calcMaps
) where

import           Control.Monad                     ((>=>))
import           Control.Monad.Trans.State         (State, get, put, runState)
import           Data.Data                         (Data)
import           Data.List                         as List (mapAccumL)
import qualified Data.Map                          as Map (Map, empty,
                                                           findWithDefault,
                                                           insert, insertWith,
                                                           lookup)
import           Data.Typeable                     (Typeable)
import           SchedulePlanner.Calculator.Solver (Lesson (..), time, timeslot)


-- |The scope and target a 'Rule' whishes to influence
data Target         = Slot Int | Day Int | Cell Int Int deriving (Show, Typeable, Data, Ord, Eq)
-- |Weight increase by 'severity' for all 'Lesson's in target
data Rule           = Rule {target :: Target, severity :: Int} deriving (Show, Typeable, Data)
-- |Dynamic rule with only one condition
data SimpleDynRule  = SimpleDynRule {sDynTarget :: Target, sDynSeverity :: Int} deriving (Show)


-- |Type alias for more expressive function signature
type WeightMap   = Map.Map Target Int
-- |Type alias for structure holding the dynamic rules
type DynRuleMap a  = Map.Map Target [a]


-- |Scaffolding for a dynamic rule
class DynamicRule a where
  trigger          :: Lesson s -> WeightMap -> a -> (WeightMap, a)
  getTriggerTarget :: a -> [Target]


instance DynamicRule SimpleDynRule where
  trigger inserted wMap (SimpleDynRule targ sev) =
    (Map.insertWith (+) targ sev wMap, SimpleDynRule targ sev)

  getTriggerTarget = return.sDynTarget


-- |Recalculate the lesson weight tuple as a result of dynamic rules
reCalcMaps :: DynamicRule a => Lesson s -> DynRuleMap a -> WeightMap -> (DynRuleMap a, WeightMap)
reCalcMaps lesson = runState .
  (reCalcHelper lesson (Slot (timeslot lesson)) >=>
    reCalcHelper lesson (Day (day lesson)) >=>
      reCalcHelper lesson (uncurry Cell (time lesson)))


reCalcHelper :: (DynamicRule a, Ord k)
    => Lesson s
    -> k
    -> Map.Map k [a]
    -> State WeightMap (Map.Map k [a])
reCalcHelper inserted key =
  pure maybe
    <*> return
    <*> (\rMap rules -> do
            s <- get
            let (newState, newRules) = mapAccumL (trigger inserted) s rules
            put newState
            return $ Map.insert key newRules rMap)
    <*> Map.lookup key


{-|
  Main function of the module.

  This funcion calculates weights the 'Lesson's provided applying the
  'Rule's provided.

  Resulting 'Lesson's are exactly the same, except for the weight
  component which is the old weight + the weight calculated from the rules
-}
weigh :: [Rule] -> [Lesson s] -> [Lesson s]
weigh = map . weighOne . calcMaps


{-|
  Weighs a single 'Lesson', but instead of 'Rule's expects a
  weight increase map.
-}
weighOne :: WeightMap -> Lesson s -> Lesson s
weighOne wm l =
  l {weight = weight l + allTargeting l wm}


allTargeting :: Lesson s -> WeightMap -> Int
allTargeting l = pure (((+) .) . (+))
  <*> Map.findWithDefault 0 (Slot $ timeslot l)
  <*> Map.findWithDefault 0 (Day $ day l)
  <*> Map.findWithDefault 0 (uncurry Cell $ time l)



{-|
  Contruct a /scope -> weight increase/ map for more efficient
  weighing afterwards
-}
calcMaps :: [Rule] -> WeightMap
calcMaps = flip calcMapsStep Map.empty


{-|
  Recursive step for the actual calculation done by 'calcMaps'
-}
calcMapsStep :: [Rule] -> WeightMap -> WeightMap
calcMapsStep []               = id
calcMapsStep (Rule t sev :xs) = calcMapsStep xs . Map.insertWith (+) t sev
