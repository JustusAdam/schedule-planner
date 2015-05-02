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

import           Calculator.Solver         (Lesson (..), time, timeslot)
import           Control.Monad.Trans.State (State, put, get, runState)
import           Data.Data                 (Data)
import           Data.List                 as List (mapAccumL)
import qualified Data.Map                  as Map (Map, empty, findWithDefault,
                                                   insert, insertWith, lookup,
                                                   update)
import           Data.Tuple                (swap)
import           Data.Typeable             (Typeable)


-- |The scope and target a 'Rule' whishes to influence
data Target         = Slot Int | Day Int | Cell Int Int deriving (Show, Typeable, Data, Ord, Eq)
-- |Weight increase by 'severity' for all 'Lesson's in target
data Rule           = Rule {target :: Target, severity :: Int} deriving (Show, Typeable, Data)
-- |Dynamic rule with only one condition
data SimpleDynRule  = SimpleDynRule {sDynTarget :: Target, sDynSeverity :: Int} deriving (Show)


-- |Type alias for more expressive function signature
type WeightMap   = Map.Map Target Int
-- |Type alias for structure holding the dynamic rules
type DynRuleTuple   = (Map.Map Int [SimpleDynRule],
                       Map.Map Int [SimpleDynRule],
                       Map.Map (Int, Int) [SimpleDynRule])


-- instance Ord Target where


-- |Scaffolding of a dynamic rule
class DynamicRule a where
  trigger          :: Lesson s -> WeightMap -> a -> (WeightMap, a)
  getTriggerTarget :: a -> [Target]


instance DynamicRule SimpleDynRule where
  trigger inserted wMap (SimpleDynRule target sev) =
    (Map.insertWith (+) target sev wMap, SimpleDynRule target sev)

  getTriggerTarget = return.sDynTarget


-- |Recalculate the lesson weight tuple as a result of dynamic rules
reCalcMaps :: WeightMap -> Lesson s -> DynRuleTuple -> (DynRuleTuple, WeightMap)
reCalcMaps weightMap lesson (slotRules, dayRules, cellRules) = runState (do

  newSlotRules  <- reCalcHelper lesson timeslot slotRules
  newDayRules   <- reCalcHelper lesson day      dayRules
  newCellRules  <- reCalcHelper lesson time     cellRules

  return (newSlotRules, newDayRules, newCellRules)) weightMap


reCalcHelper :: Ord k
    => Lesson s
    -> (Lesson s -> k)
    -> Map.Map k [SimpleDynRule]
    -> State WeightMap (Map.Map k [SimpleDynRule])
reCalcHelper inserted accessor rMap =
  case Map.lookup (accessor inserted) rMap of
    Nothing     -> return rMap
    Just rules  -> do
      s <- get
      let (newState, newRules) = mapAccumL (trigger inserted) s rules
      put newState
      return $ Map.insert (accessor inserted) newRules rMap


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
weigh :: [Rule] -> [Lesson s] -> [Lesson s]
weigh rs = map (weighOne (calcMaps rs))


{-|
  Weighs a single 'Lesson', but instead of 'Rule's expects a
  'Tuple' of weight increase maps.
-}
weighOne :: WeightMap -> Lesson s -> Lesson s
weighOne wm l =
  l {weight = weight l + allTargeting l wm}


allTargeting :: Lesson s -> WeightMap -> Int
allTargeting l = pure (\a b c -> a + b + c)
  <*> Map.findWithDefault 0 (Slot $ timeslot l)
  <*> Map.findWithDefault 0 (Day $ day l)
  <*> Map.findWithDefault 0 ((uncurry Cell) $ time l)



{-|
  Contruct a 'Tuple' of /scope -> weight increase/ maps for more efficient
  weighing afterwards
-}
calcMaps :: [Rule] -> WeightMap
calcMaps = (`calcMapsStep` Map.empty)


{-|
  Recursive step for the actual calculation done by 'calcMaps'
-}
calcMapsStep :: [Rule] -> WeightMap -> WeightMap
calcMapsStep []               = id
calcMapsStep (Rule t sev :xs) = (calcMapsStep xs).(Map.insertWith (+) t sev)
