{-# LANGUAGE OverloadedStrings #-}
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
module SchedulePlanner.Serialize where

import           Control.Applicative        (pure, (<*>))
import           Control.Arrow              as Arrow (first)
import           Data.Aeson                 (FromJSON, Object, ToJSON,
                                             Value (Object), decode, encode,
                                             object, parseJSON, toJSON, (.:),
                                             (.=))
import           Data.Aeson.Types           (Parser)
import qualified Data.ByteString            as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy       as LBS (readFile, writeFile)
import           Data.List                  as List (intercalate)
import qualified Data.Map                   as Map (Map, assocs, lookup, toList)
import           Data.String                (IsString)
import           Data.Text                  as T (Text, append, intercalate,
                                                  pack)
import           Data.Traversable           (sequenceA)
import           SchedulePlanner.Calculator
import           Text.Printf                (printf)



-- |Key for the rules data in the json input
ruleKey       = "rules"
-- |Key for the lesson data in the json input
lessonKey     = "lessons"
-- |Key for the scope property in Rule objects in the json input
scopeKey      = "scope"
-- |Key for the severity property in Rule objects in the json input
severityKey   = "severity"
-- |Key for the day property in Rule objects in the json input
ruleDayKey    = "day"
-- |Key for the slot property in Rule objects in the json input
ruleSlotKey   = "slot"
-- |Key for the subject property in Lesson objects in the json input
subjectKey    = "subject"
-- |Key for the day property in Lesson objects in the json input
lessonDayKey  = "day"
-- |Key for the slot property in Rule objects in the json input
lessonSlotKey = "slot"


-- |How many days a week has
daysPerWeek   = 7
-- |The amount of imeslots each day
slotsPerDay   = 7
-- |The caracter width of a single slot in output
cellWidth     = 20


data DataFile a = DataFile [Rule] [Lesson a]


instance FromJSON a => FromJSON (Lesson a) where
  parseJSON (Object o) =
    pure Lesson
      <*> o .: lessonSlotKey
      <*> o .: lessonDayKey
      <*> pure 0
      <*> o .: subjectKey


instance ToJSON a => ToJSON (Lesson a) where
  toJSON =
    object . sequenceA
      [ ((.=) lessonSlotKey . timeslot)
      , ((.=) lessonDayKey  . day)
      , ((.=) subjectKey    . subject)
      ]


instance ToJSON Rule where
  toJSON =
    object . (pure (:)
      <*> ((.=) severityKey . severity)
      <*> uncurry (:) . Arrow.first (scopeKey .=) . getTarget . target)
    where
      getTarget :: Target -> (Text, [(Text, Value)])
      getTarget (Day d)    = ("day", [ruleDayKey .= d])
      getTarget (Cell d s) = ("cell", [ruleDayKey .= d, ruleSlotKey .= s])
      getTarget (Slot s)   = ("slot",  [ruleSlotKey .= s])


instance FromJSON Rule where
  parseJSON (Object o) =
    pure Rule
      <*> ((o .: scopeKey) >>= (fromScope o))
      <*> o .: severityKey
    where
      fromScope :: Object -> Text -> Parser Target
      fromScope o "day"  = fmap Day (o .: ruleDayKey)
      fromScope o "slot" = fmap Slot (o .: ruleSlotKey)
      fromScope o "cell" = pure Cell <*> o .: ruleSlotKey <*> o .: ruleDayKey
      fromScope _ _      = error "unknown input"  -- I am so sorry


instance FromJSON a => FromJSON (DataFile a) where
  parseJSON (Object o) =
    pure DataFile
      <*> o .: lessonKey
      <*> o .: ruleKey

instance ToJSON a => ToJSON (DataFile a) where
  toJSON (DataFile r l) =
    object
      [ lessonKey .= l
      , ruleKey   .= r
      ]


instance ToJSON a => ToJSON (Map.Map Text a) where
  toJSON = object . map (uncurry (.=)) . Map.toList

-- |Open a file and return the contents as parsed json
getFromFile :: FromJSON a => FilePath -> IO(Maybe (DataFile a))
getFromFile = fmap decode . LBS.readFile


-- |Open a file and write json to it
writeToFile :: ToJSON a => FilePath -> DataFile a -> IO()
writeToFile filename = LBS.writeFile filename . encode


shortSubject :: Show s => s -> String
shortSubject = reverse . take cellWidth . reverse . show


{-|
  Transform a 'MappedSchedule' into a printable,
  and more importantly, readable Text
-}
formatSchedule :: Show s => MappedSchedule s -> Text
formatSchedule hours = pack $ List.intercalate "\n" $ header : map formatDay allHours
  where
    allHours = [(i, [1..slotsPerDay]) | i <- [1..daysPerWeek]]

    formatLesson :: Timeslot -> String
    formatLesson i =
      printf ("%" ++ show cellWidth ++ "v") $ maybe [] (shortSubject . subject) (Map.lookup i hours)

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = List.intercalate " | " [formatLesson (j, i) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight hours)
