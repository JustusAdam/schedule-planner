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
import           Control.Monad              (liftM)
import           Data.List                  (intercalate)
import qualified Data.Map                   as Map (assocs, lookup)
import           SchedulePlanner.Calculator
import           Text.JSON                  as JSON (JSON, JSObject, JSValue (JSArray, JSObject),
                                                     Result (..), decodeStrict,
                                                     encode, showJSON,
                                                     toJSObject, valFromObj)
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


-- |Open a file and return the contents as parsed json
getFromFile :: JSON a => String -> IO(Result a)
getFromFile filename = liftM decodeStrict (readFile filename)


-- |Open a file and write json to it
writeToFile :: String -> JSValue -> IO()
writeToFile filename = writeFile filename . JSON.encode


-- |Turns parsed json values into the internally used datastructures.
deSerialize :: Result JSValue -> Result ([Result Rule], [Result (Lesson String)])
deSerialize (Ok (JSObject o))  = do
    rv      <- valFromObj ruleKey o
    lv      <- valFromObj lessonKey o

    rules   <- extractRules rv
    lessons <- extractLessons lv

    return (rules, lessons)
deSerialize (Ok _)             = Error "wrong value type"
deSerialize (Error e)          = Error e


serialize :: Show s => [MappedSchedule s] -> String
serialize = JSON.encode . nativeToJson


-- |Transform Native the native schedules into JSON
nativeToJson :: Show s => [MappedSchedule s] -> JSValue
nativeToJson = JSArray . map convert
  where
    convert :: Show s => MappedSchedule s -> JSValue
    convert =
      pure (\a b -> JSObject (JSON.toJSObject [a,b]))
        <*> ((,) "weight" . showJSON . totalWeight)
        <*> ((,) "values" . JSArray .
              map
                (\((i, j), b) ->
                  JSObject (JSON.toJSObject [
                            ("day", showJSON i),
                            ("slot", showJSON j),
                            ("subject", (showJSON . show . subject) b)
                          ]))
                . Map.assocs
              )


-- |Turns a parsed json value into a 'List' of 'Rule's or return an 'Error'
extractRules :: JSValue -> Result [Result Rule]
extractRules (JSArray rv)  = return $ map handleOne rv
  where
    handleOne :: JSValue -> Result Rule
    handleOne (JSObject o)  = do
      scope     <- valFromObj scopeKey o
      severity  <- valFromObj severityKey o

      let rp = (`Rule` severity)

      case scope of
        "day"   -> do
          day   <- valFromObj ruleDayKey o
          return $ rp $ Day day
        "slot"  -> do
          slot  <- valFromObj ruleSlotKey o
          return $ rp $ Slot slot
        "cell"  -> do
          slot  <- valFromObj ruleSlotKey o
          day   <- valFromObj ruleDayKey o
          return $ rp $ Cell day slot

    handleOne _             = Error "wrong value type"

extractRules _             = Error "key lessons does not contain array"


-- |Turns a parsed json value into a 'List' of 'Lesson's or return an 'Error'
extractLessons :: JSValue -> Result [Result (Lesson String)]
extractLessons (JSArray a)  = return $ map handleOne a
  where
    handleOne :: JSValue -> Result (Lesson String)
    handleOne (JSObject o)  = do
      subject <- valFromObj subjectKey o
      day     <- valFromObj lessonDayKey o
      slot    <- valFromObj lessonSlotKey o
      return $ Lesson slot day 0 subject
    handleOne _             = Error "wrong type"

extractLessons _            = Error "wrong value type"


shortSubject :: Show s => s -> String
shortSubject = reverse . take cellWidth . reverse . show


{-|
  Transform a 'MappedSchedule' into a printable,
  and more importantly, readable String
-}
formatSchedule :: Show s => MappedSchedule s -> String
formatSchedule hours = intercalate "\n" $ header : map formatDay allHours
  where
    allHours = [(i, [1..slotsPerDay]) | i <- [1..daysPerWeek]]

    formatLesson :: Timeslot -> String
    formatLesson i =
      printf ("%" ++ show cellWidth ++ "v") $ maybe [] (shortSubject . subject) (Map.lookup i hours)

    formatDay :: (Int, [Int]) -> String
    formatDay (i, l) = intercalate " | " [formatLesson (j, i) | j <- l]

    header = printf "Total Weight: %10v" (totalWeight hours)
