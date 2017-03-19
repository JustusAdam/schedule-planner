{-# LANGUAGE UnicodeSyntax #-}
{-|
Module      : $Header$
Description : Basic types used internally by this software
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Custom types the software uses.
-}
module SchedulePlanner.Types
  ( Slot(..)
  , Day(..)
  , Cell(..)
  , Lesson(..)
  , Target(..)
  , Rule(..)
  , SimpleDynRule(..)
  , Weight
  ) where


import           Data.Typeable
import ClassyPrelude hiding (Day)


newtype Slot = Slot { unSlot :: Int } deriving (Eq, Show, Ord)

newtype Day  = Day  { unDay :: Int } deriving (Eq, Show, Ord)

newtype Cell = Cell { unCell :: (Day, Slot) } deriving (Eq, Show, Ord)

type Weight = Integer


-- | Base datastructure for representing lessons
data Lesson s = Lesson { timeslot :: Slot
                       , day      :: Day
                       , subject  :: s
                       } deriving (Show, Eq, Ord, Typeable)


-- | The scope and target a 'Rule' whishes to influence
data Target = TSlot Slot
            | TDay Day
            | TCell Cell
            deriving (Show, Typeable, Ord, Eq)


-- | Weight increase by 'severity' for all 'Lesson's in target
data Rule = Rule { target   :: Target
                 , severity :: Int
                 } deriving (Show, Typeable)


-- | Dynamic rule with only one condition
data SimpleDynRule = SimpleDynRule { sDynTarget   :: Target
                                   , sDynSeverity :: Int
                                   } deriving (Show)
