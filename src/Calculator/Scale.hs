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

import           Calculator.Solver (Lesson (..), time, timeslot)
import           Control.Monad
import           Data.Data         (Data)
import           Data.List         as List (mapAccumL)
import qualified Data.Map          as Map (Map, empty, findWithDefault, insert,
                                           insertWith, lookup, update)
import           Data.Tuple        (swap)
import           Data.Typeable     (Typeable)
import Control.Monad.Trans.State


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
  trigger          :: Lesson s -> WeightMapTuple -> a -> (WeightMapTuple, a)
  getTriggerTarget :: a -> [Target]


instance DynamicRule SimpleDynRule where
  trigger inserted (ms, md, mc) (SimpleDynRule target sev) = (

    case target of
      Slot slot   ->  (ms, Map.insertWith (+) slot sev md, mc)
      Day  day    ->  (Map.insertWith (+) day sev ms, md, mc)
      Cell c1 c2  ->  (ms, md, Map.insertWith (+) (c1, c2) sev mc)

    , SimpleDynRule target sev)

  getTriggerTarget = return.sDynTarget


-- |Recalculate the lesson weight tuple as a result of dynamic rules
reCalcMaps :: WeightMapTuple -> Lesson s -> DynRuleTuple -> (DynRuleTuple, WeightMapTuple)
reCalcMaps wmt l (rs, rd, rc) = runState (do
  rsn <- reCalcHelper l timeslot rs
  rdn <- reCalcHelper l day rd
  rcn <- reCalcHelper l time rc
  return (rsn, rdn, rcn)) wmt


reCalcHelper :: Ord k
    => Lesson s
    -> (Lesson s -> k)
    -> Map.Map k [SimpleDynRule]
    -> State WeightMapTuple (Map.Map k [SimpleDynRule])
reC inserted accessor rMap =
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
weighOne :: WeightMapTuple -> Lesson s -> Lesson s
weighOne (ms, md, mc) l =
  l {
      weight =
        weight l
        + Map.findWithDefault 0 (timeslot l) ms
        + Map.findWithDefault 0 (day l) md
        + Map.findWithDefault 0 (time l) mc
    }


{-|
  Contruct a 'Tuple' of /scope -> weight increase/ maps for more efficient
  weighing afterwards
-}
calcMaps :: [Rule] -> WeightMapTuple
calcMaps = (`calcMapsStep` (Map.empty, Map.empty, Map.empty))


{-|
  Recursive step for the actual calculation done by 'calcMaps'
-}
calcMapsStep :: [Rule] -> WeightMapTuple -> WeightMapTuple
calcMapsStep [] mt                          = mt
calcMapsStep (Rule t sev :xs) (ms, md, mc)  = calcMapsStep xs newMaps

  where
    increase :: Ord k => k -> Map.Map k Int -> Map.Map k Int
    increase target = Map.insertWith (+) target sev

    newMaps = case t of
                Slot s      -> (increase s ms, md, mc)
                Day d       -> (ms, increase d md, mc)
                Cell c1 c2  -> (ms, md, increase (c1, c2) mc)
