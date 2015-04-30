{-|
Module      : $Header$
Description : Interface for schedule-planner
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

main function and outside communication for this software.
This module takes care of reading all input and checking for correctness
as well as providing useful feedback upon encountering errors.
-}
module Main where

import           Calculator.Scale
import           Calculator.Solver
import           Control.Applicative
import           Control.Monad
import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import           Options
import           System.Environment
import           System.IO
import           Text.JSON           as JSON


-- |Enables debug messages
debugMode = True
-- |Temporary constant, should be in call args eventually
outputFormatDefault = "print"


-- |Legacy hard coded name of inputfile
stdFileName   = "testsuite/test.json"
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


data CallOptions = CallOptions {
    outputFile   :: Maybe String,
    inputFile    :: String,
    outputFormat :: String
  } deriving (Show)

instance Options CallOptions where
  defineOptions = pure CallOptions
    <*> defineOption (optionType_maybe optionType_string) (\o -> o {
        optionLongFlags   = ["output-file"],
        optionShortFlags  = "o",
        optionDescription = "print output to this file instead of stdout",
        optionDefault     = Nothing
      })
    <*> defineOption optionType_string (\o -> o {
        optionDefault     = stdFileName,
        optionLongFlags   = ["input-file"],
        optionShortFlags  = "i",
        optionDescription = "read input from this file"
      })
    <*> defineOption optionType_string (\o -> o {
        optionDefault     = outputFormatDefault,
        optionLongFlags   = ["output-format"],
        optionShortFlags  = "f",
        optionDescription = "set the output format"
      })


-- |Legacy test data
testLessons   = [
        Lesson 1 1 2 "TGI",
        Lesson 1 1 2 "FS",
        Lesson 1 1 1 "TGI",
        Lesson 2 2 3 "TGI",
        Lesson 1 1 3 "RA",
        Lesson 2 2 2 "RA",
        Lesson 1 2 3 "FS"
      ]


-- |Print a line to stdout
putErrorLine :: String -> IO()
putErrorLine = hPutStrLn stderr


-- |Open a file and return the contents as parsed json
getFromFile :: JSON a => String -> IO(Result a)
getFromFile filename =
  liftM decodeStrict (readFile filename)


-- |Open a file and write json to it
writeToFile :: String -> JSValue -> IO()
writeToFile filename = writeFile filename . JSON.encode


-- |Turns parsed json values into the internally used datastructures.
toNative :: Result JSValue -> Result ([Result Rule], [Result Lesson])
toNative (Ok (JSObject o))  = do
    rv      <- valFromObj ruleKey o
    lv      <- valFromObj lessonKey o

    rules   <- extractRules rv
    lessons <- extractLessons lv

    return (rules, lessons)
toNative (Ok _)             = Error "wrong value type"
toNative (Error e)          = Error e


-- |Transform Native the native schedules into JSON
fromNative :: [MappedSchedule] -> JSValue
fromNative = JSArray . liftM convert
  where
    convert :: MappedSchedule -> JSValue
    convert = pure (\a b -> JSObject (JSON.toJSObject [a,b]))
        <*> ((,) "weight" . showJSON . totalWeight)
        <*> ((,) "values" . JSArray .
              map
                (\((i, j), b) ->
                  JSObject (JSON.toJSObject [
                            ("day", showJSON i),
                            ("slot", showJSON j),
                            ("subject", showJSON (subject b))
                          ]))
                . Map.assocs
              )


-- |Turns a parsed json value into a 'List' of 'Rule's or return an 'Error'
extractRules :: JSValue -> Result [Result Rule]
extractRules (JSArray rv)  =
  return $ map handleOne rv

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


-- |Print a string if debug is enabled
printDebug :: Show a => a -> IO()
printDebug = when debugMode . print


-- |Turns a parsed json value into a 'List' of 'Lesson's or return an 'Error'
extractLessons :: JSValue -> Result [Result Lesson]
extractLessons (JSArray a)  =
  return $ map handleOne a

  where
    handleOne :: JSValue -> Result Lesson
    handleOne (JSObject o)  = do
      subject <- valFromObj subjectKey o
      day     <- valFromObj lessonDayKey o
      slot    <- valFromObj lessonSlotKey o
      return $ Lesson slot day 0 subject
    handleOne _             = Error "wrong type"

extractLessons _            = Error "wrong value type"


{-|
  Evaluates the transformed json, compiles (useful) error messages, prints them
  and then runs the algorithm or, if the errors are too severe, abourts.
-}
reportAndExecute :: String -> Result ([Result Rule], [Result Lesson]) -> IO()
reportAndExecute _ (Error s)    =
  putErrorLine $ "Stopped execution due to a severe problem with the input data:" ++ show s
reportAndExecute outputFormat (Ok (r, l))  = do
  rules   <- reportOrReturn r
  lessons <- reportOrReturn l

  let weighted      = weigh rules lessons

  let mappedLessons = mapToSubject weighted

  let result        = calcFromMap mappedLessons

  case result of
    Nothing ->
      putStrLn "Calculation failed, no valid schedule possible"
    Just calculated ->

      case outputFormat of

        "print" -> do

          putStrLn "\n"
          _       <- mapM printDebug rules
          putStrLn "\n"

          putStrLn "\n"
          _       <- mapM printDebug weighted
          putStrLn "\n"

          putStrLn "Legend:"
          _       <- mapM (print . (\ x -> (List.take 10 x, x))) (Map.keys mappedLessons)


          putStrLn "\n"
          _       <- pc calculated
          return ()

        "json" -> do
          print $ JSON.encode (fromNative calculated)
          return ()


  where
    pc = mapM (\x -> putStrLn ("\n\n" ++ formatSchedule x))

    reportOrReturn :: [Result a] -> IO [a]
    reportOrReturn []     =
      return []
    reportOrReturn (x:xs) =
      case x of
        Error s -> do
          putErrorLine "Some data was unusable:"
          putErrorLine s
          reportOrReturn xs
        Ok v    -> do
          ps <- reportOrReturn xs
          return (v:ps)


{-|
  main function. Handles reading command line arguments, the json input
  and starts execution.
-}
main :: IO()
main = runCommand $ \opts args -> do
  rawInput <- getFromFile $ inputFile opts

  reportAndExecute (outputFormat opts) (toNative rawInput)
  -- printDebug opts
  -- putStrLn (foldl (++) "" args)
  -- print (outputFormat opts)
