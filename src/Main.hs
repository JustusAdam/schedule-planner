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
import qualified Data.List          as List
import qualified Data.Map           as Map
import           System.Environment
import           System.IO
import           Text.JSON          as JSON
import Control.Monad


debugMode = True
outputFormat = "print"


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
getFromFile filename = do
  string <- readFile filename
  return $ decodeStrict string


writeToFile :: String -> JSValue -> IO()
writeToFile filename v =
  writeFile filename $ JSON.encode v


-- |Turns parsed json values into the internally used datastructures.
toNative :: Result JSValue -> Result ([Result Rule], [Result Lesson])
toNative i = do
  v <- i
  inner v

  where
    inner :: JSValue -> Result ([Result Rule], [Result Lesson])
    inner (JSObject o)  = do
      rv      <- valFromObj ruleKey o
      lv      <- valFromObj lessonKey o

      rules   <- extractRules rv
      lessons <- extractLessons lv

      return (rules, lessons)
    inner _             = Error "wrong value type"


fromNative :: [MappedSchedule] -> JSValue
fromNative m = JSArray (liftM (JSArray . convert) m)
  where
    convert :: MappedSchedule -> [JSValue]
    convert e = do
      ((i,j),b) <- Map.assocs e
      return $ JSObject (JSON.toJSObject [
                          ("day", showJSON i),
                          ("slot", showJSON j),
                          ("subject", showJSON (subject b))
                          ])


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


printDebug :: Show a => a -> IO()
printDebug o =
  when debugMode $ print o


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
reportAndExecute :: Result ([Result Rule], [Result Lesson]) -> IO()
reportAndExecute (Error s)    =
  putErrorLine $ "Stopped execution due to a severe problem with the input data:" ++ show s
reportAndExecute (Ok (r, l))  = do
  rules   <- reportOrReturn r
  lessons <- reportOrReturn l

  putStrLn "\n"
  _       <- mapM print rules
  putStrLn "\n"

  let weighted      = weigh rules lessons

  putStrLn "\n"
  _       <- mapM print weighted
  putStrLn "\n"

  let mappedLessons = mapToSubject weighted

  let calculated    = calcFromMap mappedLessons

  case outputFormat of

    "print" -> do
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
main = do
  args <- getArgs
  let filename = head args
  json <- getFromFile filename
  let native = toNative json
  reportAndExecute native
