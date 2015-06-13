{-|
Module      : $Header$
Description : base module for the mathematical operations
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Exports the most important functions neccessary for the actual work
of this software
-}
module SchedulePlanner.Calculator
  ( calcFromMap
  , calcFromList
  , totalWeight
  , Lesson (..)
  , Cell(..)
  , Slot(..)
  , Day(..)
  , Target(..)
  , Rule(..)
  , mapToSubject
  , MappedSchedule(..)
  , MappedLessons(..)
  , weigh
) where

import SchedulePlanner.Calculator.Scale
import SchedulePlanner.Calculator.Solver
