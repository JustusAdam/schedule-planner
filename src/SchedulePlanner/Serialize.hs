{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-|
Module      : $Header$
Description : (de)serializing in- and output
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Hold the capeablilities to get and export in- and output data as well as (de)serialize it
-}
module SchedulePlanner.Serialize
  ( mapToJSON
  , formatSchedule
  , shortSubject
  , DataFile(DataFile)
  , eitherDecode
  , scheduleToJson
  ) where

import           Control.Applicative.Unicode
import           Control.Arrow                as Arrow (first, second)
import           Control.Arrow.Unicode
import           Control.Monad                (mzero)
import           Control.Monad.Unicode
import           Data.Aeson                   (FromJSON, Object, ToJSON,
                                               Value (Object), eitherDecode,
                                               object, parseJSON, toJSON, (.:),
                                               (.=))
import           Data.Aeson.Types             (Parser)
import qualified Data.Composition             as Comp ((.:))
import           Data.List                    as List (intercalate)
import qualified Data.Map                     as Map (Map, elems, lookup,
                                                      toList)
import           Data.Text                    as T (Text, pack, unpack)
import           Prelude.Unicode
import           SchedulePlanner.Calculator   (MappedSchedule (..), totalWeight)
import           SchedulePlanner.Scraper.Base
import           SchedulePlanner.Types
import           Text.Printf                  (printf)


-- | How many days a week has
daysPerWeek   :: Int
daysPerWeek   = 7
-- | The amount of imeslots each day
slotsPerDay   :: Int
slotsPerDay   = 7
-- | The caracter width of a single slot in output
cellWidth     :: Int
cellWidth     = 20


-- |Base structure of the input JSON file
data DataFile = DataFile [Rule] [Lesson Text] deriving (Show)


instance FromJSON a => FromJSON (Lesson a) where
  parseJSON (Object o) = Lesson
    <$> (Slot <$> o .: "slot")
    ⊛ (Day  <$> o .: "day")
    ⊛ return 0
    ⊛ o .:  "subject"
  parseJSON _          = mzero


instance ToJSON a => ToJSON (Lesson a) where
  toJSON =
    object . sequence
      [ (.=) "slot"    . unSlot . timeslot
      , (.=) "day"     . unDay . day
      , (.=) "subject" . subject
      ]


instance ToJSON Rule where
  toJSON =
    object . ((:)
      <$> ((.=) "severity" . severity)
      ⊛ uncurry (:) . Arrow.first ("scope" .=) . getTarget . target)
    where
      getTarget :: Target -> (Text, [(Text, Value)])
      getTarget (TDay d)    = ("day", ["day"  .= unDay d])
      getTarget (TCell c)   =
        second
          ( sequence
            [ ("day"  .=) . unDay . fst
            , ("slot" .=) . unSlot . snd
            ])
          ("cell", unCell c)
      getTarget (TSlot s)   = ("slot", ["slot" .= unSlot s])


instance FromJSON Rule where
  parseJSON (Object o) = Rule
    <$> ((o .: "scope") ≫= fromScope o)
    ⊛ o .: "severity"
    where
      fromScope :: Object -> Text -> Parser Target
      fromScope obj "day" = (TDay  . Day)  <$> obj .: "day"
      fromScope obj "slot" = (TSlot . Slot) <$> obj .: "slot"
      fromScope obj "cell" = ((TCell . Cell) Comp..: curry (Day ⁂ Slot))
                                      <$> obj .: "day"
                                      ⊛ obj .: "slot"
      fromScope _ scope = error $ "unknown scope " ++ unpack scope  -- I am so sorry
  parseJSON _         = mzero


instance FromJSON DataFile where
  parseJSON (Object o) = DataFile
    <$> o .: "rules"
    ⊛ o .: "lessons"
  parseJSON _          = mzero


instance ToJSON DataFile where
  toJSON (DataFile r l) =
    object
      [ "lessons" .= l
      , "rules"   .= r
      ]


instance ToJSON Semester where
  toJSON = toJSON . unSemester


{-|
  Convert a single 'MappedSchedule' to a JSON 'Value'
-}
scheduleToJson :: ToJSON a => MappedSchedule a -> Value
scheduleToJson = object . sequence
  [ (.=) "lessons" . Map.elems . unMapSchedule
  , (.=) "weight" . totalWeight
  ]


-- |Convert a suitable Map to a JSON Value
mapToJSON :: ToJSON a => Map.Map Text a -> Value
mapToJSON = object . map (uncurry (.=)) . Map.toList


-- |Shorten a subject to something printable
shortSubject :: Show s => s -> String
shortSubject = reverse . take cellWidth . reverse . show


{-|
  Transform a 'MappedSchedule' into a printable,
  and more importantly, readable Text
-}
formatSchedule :: Show s => MappedSchedule s -> Text
formatSchedule (MappedSchedule hours) = pack $ List.intercalate "\n" $ header : map formatDay allHours
  where
    allHours = [(i, [1..slotsPerDay]) | i <- [1..daysPerWeek]]

    formatLesson :: Cell -> String
    formatLesson i =
      printf ("%" ++ show cellWidth ++ "v") $ maybe [] (shortSubject . subject) (Map.lookup i hours)

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = List.intercalate " | " [formatLesson $ Cell (Day j, Slot i) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight (MappedSchedule hours))
