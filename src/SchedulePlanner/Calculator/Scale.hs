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
  , WeightMap
  ) where

import           Control.Applicative.Unicode
import           Control.Arrow                     (second)
import           Control.Monad                     ((>=>))
import           Control.Monad.Trans.State         (State, get, put, runState)
import           Data.Composition                  ((.:))
-- import           Data.List                         as List (mapAccumL)
import ClassyPrelude
import           SchedulePlanner.Calculator.Solver (time)
import           SchedulePlanner.Types


-- | Type alias for more expressive function signature
type WeightMap    = (IsMap map, ContainerKey map ~ Target, MapValue map ~ Int) => map


-- | Type alias for structure holding the dynamic rules
type DynRuleMap a = (IsMap map, ContainerKey map ~ Target, MapValue map ~ [a]) => map


-- | Scaffolding for a dynamic rule
class DynamicRule r where
  trigger          :: Lesson s -> WeightMap -> r -> (WeightMap, r)
  getTriggerTarget :: r -> [Target]


instance DynamicRule SimpleDynRule where
  trigger _ wMap (SimpleDynRule targ sev) =
    (insertWith (+) targ sev wMap, SimpleDynRule targ sev)

  getTriggerTarget = return . sDynTarget


-- | Recalculate the lesson weight tuple as a result of dynamic rules
reCalcMaps :: DynamicRule δ
           => Lesson s
           -> DynRuleMap δ
           -> WeightMap
           -> (DynRuleMap δ, WeightMap)
reCalcMaps lesson = runState .
  (reCalcHelper lesson (TSlot (timeslot lesson)) >=>
    reCalcHelper lesson (TDay (day lesson)) >=>
      reCalcHelper lesson (TCell (time lesson)))


{-|
  Stateful recalculation of the rule map triggered by a change.
-}
reCalcHelper :: DynamicRule δ
             => Lesson s
             -> Target
             -> DynRuleMap δ
             -> State WeightMap (DynRuleMap δ)
reCalcHelper inserted key =
  maybe <$> return <*> f <*> lookup key
  where
    f rMap rules = do
      currentWeightMap <- get
      let (newState, newRules) = mapAccumL (trigger inserted) currentWeightMap rules
      put newState
      return $ insert key newRules rMap


nonStateReCalcMaps :: DynamicRule δ
                   => Lesson s
                   -> WeightMap
                   -> DynRuleMap δ
                   -> (WeightMap, DynRuleMap δ)
nonStateReCalcMaps lesson wm drm =
  foldr (nonStateReCalcHelper lesson) (wm, drm) (constrTargets lesson)


nonStateReCalcHelper :: DynamicRule a
                     => Lesson s
                     -> Target
                     -> (WeightMap, DynRuleMap a)
                     -> (WeightMap, DynRuleMap a)
nonStateReCalcHelper inserted key (wm, drm) =
  maybe (wm, pdrm) f $ Map.lookup key drm
  where
    f = second (flip (insert key) drm) . mapAccumL (trigger inserted) wm


constrTargets :: Lesson a -> [Target]
constrTargets = sequenceA
      [ TSlot . timeslot
      , TDay . day
      , TCell . time
      ]


{-|
  Main function of the module.

  This funcion calculates weights the 'Lesson's provided applying the
  'Rule's provided.

  Resulting 'Lesson's are exactly the same, except for the weight
  component which is the old weight + the weight calculated from the rules
-}
weigh :: [Rule] -> [Lesson s] -> [(Lesson s, Weight)]
weigh = map . weighOne . calcMaps


{-|
  Weighs a single 'Lesson', but instead of 'Rule's expects a
  weight increase map.
-}
weighOne :: WeightMap -> Lesson s -> Lesson s
weighOne wm l =
  l {weight = weight l + allTargeting l wm}


{-|
  Find the full weight impact from the rules on a specific lesson.
-}
allTargeting :: Lesson s -> WeightMap -> Int
allTargeting = (sum .: (sequenceA <<.> unWeightMap)) .
  map (findWithDefault 0) . constrTargets
  where
    f <<.> g = \a -> f a . g


{-|
  Contruct a /scope -> weight increase/ map for more efficient
  weighing afterwards
-}
calcMaps :: [Rule] -> WeightMap
calcMaps = flip calcMapsStep mempty


{-|
  Recursive step for the actual calculation done by 'calcMaps'
-}
calcMapsStep :: [Rule] -> WeightMap -> WeightMap
calcMapsStep []               = id
calcMapsStep (Rule t sev :xs) = calcMapsStep xs . insertWith (+) t sev
