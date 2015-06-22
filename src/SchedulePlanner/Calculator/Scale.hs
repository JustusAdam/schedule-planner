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

import           Control.Arrow                     (second)
import           Control.Monad                     ((>=>))
import           Control.Monad.Trans.State         (State, get, put, runState)
import           Data.Composition                  ((.:))
import           Data.List                         as List (mapAccumL)
import qualified Data.Map                          as Map (Map, empty,
                                                           findWithDefault,
                                                           insert, insertWith,
                                                           lookup)
import           SchedulePlanner.Calculator.Solver (time)
import           SchedulePlanner.Types


-- | Type alias for more expressive function signature
newtype WeightMap    = WeightMap { unWeightMap :: Map.Map Target Int }


-- | Type alias for structure holding the dynamic rules
newtype DynRuleMap a = DynRuleMap { unDynRuleMap :: Map.Map Target [a] }


-- | Scaffolding for a dynamic rule
class DynamicRule r where
  trigger          :: Lesson s -> WeightMap -> r -> (WeightMap, r)
  getTriggerTarget :: r -> [Target]


instance DynamicRule SimpleDynRule where
  trigger _ (WeightMap wMap) (SimpleDynRule targ sev) =
    (WeightMap $ Map.insertWith (+) targ sev wMap, SimpleDynRule targ sev)

  getTriggerTarget = return.sDynTarget


-- | Recalculate the lesson weight tuple as a result of dynamic rules
reCalcMaps :: DynamicRule a
           => Lesson s
           -> DynRuleMap a
           -> WeightMap
           -> (DynRuleMap a, WeightMap)
reCalcMaps lesson = runState .
  (reCalcHelper lesson (TSlot (timeslot lesson)) >=>
    reCalcHelper lesson (TDay (day lesson)) >=>
      reCalcHelper lesson (TCell (time lesson)))


{-|
  Stateful recalculation of the rule map triggered by a change.
-}
reCalcHelper :: DynamicRule a
             => Lesson s
             -> Target
             -> DynRuleMap a
             -> State WeightMap (DynRuleMap a)
reCalcHelper inserted key =
  maybe <$> return <*> f . unDynRuleMap <*> Map.lookup key . unDynRuleMap
  where
    f rMap rules = do
      currentWeightMap <- get
      let (newState, newRules) = mapAccumL (trigger inserted) currentWeightMap rules
      put newState
      return $ DynRuleMap $ Map.insert key newRules rMap


nonStateReCalcMaps :: DynamicRule a
                   => Lesson s
                   -> WeightMap
                   -> DynRuleMap a
                   -> (WeightMap, DynRuleMap a)
nonStateReCalcMaps lesson wm drm =
  foldr (nonStateReCalcHelper lesson) (wm, drm) (constrTargets lesson)  


nonStateReCalcHelper :: DynamicRule a
                     => Lesson s
                     -> Target
                     -> (WeightMap, DynRuleMap a)
                     -> (WeightMap, DynRuleMap a)
nonStateReCalcHelper inserted key (wm, pdrm@(DynRuleMap drm)) =
  maybe (wm, pdrm) f $ Map.lookup key drm
  where
    f = second (DynRuleMap . flip (Map.insert key) drm) . mapAccumL (trigger inserted) wm


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
weigh :: [Rule] -> [Lesson s] -> [Lesson s]
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
  map (Map.findWithDefault 0) . constrTargets
  where
    f <<.> g = \a -> f a . g


{-|
  Contruct a /scope -> weight increase/ map for more efficient
  weighing afterwards
-}
calcMaps :: [Rule] -> WeightMap
calcMaps = flip calcMapsStep (WeightMap Map.empty)


{-|
  Recursive step for the actual calculation done by 'calcMaps'
-}
calcMapsStep :: [Rule] -> WeightMap -> WeightMap
calcMapsStep []               = id
calcMapsStep (Rule t sev :xs) = calcMapsStep xs . WeightMap . Map.insertWith (+) t sev . unWeightMap
