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
module SchedulePlanner.Serialize
  ( mapToJSON
  , formatSchedule
  , shortSubject
  , DataFile(DataFile)
  , eitherDecode
  , scheduleToJson
  ) where

import           Control.Arrow              as Arrow (first)
import           Data.Aeson                 (FromJSON, Object, ToJSON,
                                             Value (Object), decode, encode,
                                             object, parseJSON, toJSON, (.:),
                                             (.=), eitherDecode)
import           Data.Aeson.Types           (Parser)
import qualified Data.ByteString.Lazy       as LBS (readFile, writeFile)
import           Data.List                  as List (intercalate)
import qualified Data.Map                   as Map (Map, lookup, toList, elems)
import           Data.Text                  as T (Text, pack)
import           SchedulePlanner.Calculator (Lesson (..), MappedSchedule,
                                             Rule (..), Target (..), Timeslot,
                                             timeslot, totalWeight)
import           Text.Printf                (printf)
import           Control.Monad              (mzero)



-- | Key for the rules data in the json input
ruleKey       :: Text
ruleKey       = "rules"
-- | Key for the lesson data in the json input
lessonKey     :: Text
lessonKey     = "lessons"
-- | Key for the scope property in Rule objects in the json input
scopeKey      :: Text
scopeKey      = "scope"
-- | Key for the severity property in Rule objects in the json input
severityKey   :: Text
severityKey   = "severity"
-- | Key for the day property in Rule objects in the json input
ruleDayKey    :: Text
ruleDayKey    = "day"
-- | Key for the slot property in Rule objects in the json input
ruleSlotKey   :: Text
ruleSlotKey   = "slot"
-- | Key for the subject property in Lesson objects in the json input
subjectKey    :: Text
subjectKey    = "subject"
-- | Key for the day property in Lesson objects in the json input
lessonDayKey  :: Text
lessonDayKey  = "day"
-- | Key for the slot property in Rule objects in the json input
lessonSlotKey :: Text
lessonSlotKey = "slot"


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
    <$> o .: lessonSlotKey
    <*> o .: lessonDayKey
    <*> pure 0
    <*> o .: subjectKey
  parseJSON _          = mzero


instance ToJSON a => ToJSON (Lesson a) where
  toJSON =
    object . sequenceA
      [ (.=) lessonSlotKey . timeslot
      , (.=) lessonDayKey  . day
      , (.=) subjectKey    . subject
      ]


instance ToJSON Rule where
  toJSON =
    object . ((:)
      <$> ((.=) severityKey . severity)
      <*> uncurry (:) . Arrow.first (scopeKey .=) . getTarget . target)
    where
      getTarget :: Target -> (Text, [(Text, Value)])
      getTarget (Day d)    = ("day",   [ruleDayKey  .= d])
      getTarget (Cell d s) = ("cell",  [ruleDayKey  .= d, ruleSlotKey .= s])
      getTarget (Slot s)   = ("slot",  [ruleSlotKey .= s])


instance FromJSON Rule where
  parseJSON (Object o) = Rule
    <$> ((o .: scopeKey) >>= fromScope o)
    <*> o .: severityKey
    where
      fromScope :: Object -> Text -> Parser Target
      fromScope obj "day"  = Day  <$> obj .: ruleDayKey
      fromScope obj "slot" = Slot <$> obj .: ruleSlotKey
      fromScope obj "cell" = Cell <$> obj .: ruleDayKey <*> obj .: ruleSlotKey
      fromScope _   _      = error "unknown input"  -- I am so sorry
  parseJSON _         = mzero


instance FromJSON DataFile where
  parseJSON (Object o) = DataFile
    <$> o .: ruleKey
    <*> o .: lessonKey
  parseJSON _          = mzero


instance ToJSON DataFile where
  toJSON (DataFile r l) =
    object
      [ lessonKey .= l
      , ruleKey   .= r
      ]


{-|
  Convert a single 'MappedSchedule' to a JSON 'Value'
-}
scheduleToJson :: ToJSON a => MappedSchedule a -> Value
scheduleToJson = object . sequenceA
  [ (.=) lessonKey . Map.elems
  , (.=) "weight"  . totalWeight
  ]


-- |Convert a suitable Map to a JSON Value
mapToJSON :: ToJSON a => Map.Map Text a -> Value
mapToJSON = object . map (uncurry (.=)) . Map.toList

-- |Open a file and return the contents as parsed json
getFromFile :: FilePath -> IO(Maybe DataFile)
getFromFile = fmap decode . LBS.readFile


-- |Open a file and write json to it
writeToFile :: ToJSON a => FilePath -> Map.Map Text a -> IO()
writeToFile filename = LBS.writeFile filename . encode . mapToJSON


-- |Shorten a subject to something printable
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