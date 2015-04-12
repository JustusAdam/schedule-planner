{-|
Module      : $Header$
Description : Interface for schedule-planner
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

main function and outside communication for this software
-}
module Main where

import           Calculator.Scale
import           Calculator.Solver
import           System.Environment
import           Text.JSON          as JSON


stdFileName   = "testsuite/test.json"
ruleKey       = "rules"
lessonKey     = "lessons"
scopeKey      = "scope"
severityKey   = "severity"
ruleDayKey    = "day"
ruleSlotKey   = "slot"
subjectKey    = "subject"
lessonDayKey  = "day"
lessonSlotKey = "slot"


testLessons = [
        Lesson 1 1 2 "TGI",
        Lesson 1 1 2 "FS",
        Lesson 1 1 1 "TGI",
        Lesson 2 2 3 "TGI",
        Lesson 1 1 3 "RA",
        Lesson 2 2 2 "RA",
        Lesson 1 2 3 "FS"
      ]


getFromFile :: JSON a => String -> IO(Result a)
getFromFile filename = do
  string <- readFile filename
  return $ decodeStrict string


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
    inner _             = Error ("wrong value type")


extractRules :: JSValue -> Result [Result Rule]
extractRules (JSArray rv)  = do
  return (map handleOne rv)

  where
    handleOne :: JSValue -> Result Rule
    handleOne (JSObject o)  = do
      scope     <- valFromObj scopeKey o
      severity  <- valFromObj severityKey o

      let rp = \x -> Rule x severity


      day       <- valFromObj ruleDayKey o
      slot      <- valFromObj ruleSlotKey o

      case scope of
        "day" ->
          return $ rp $ Day day
        "slot" ->
          return $ rp $ Slot slot
        "cell" ->
          return $ rp $ Cell day slot

    handleOne _           = Error "wrong value type"

extractRules _          = Error "key lessons does not contain array"


extractLessons :: JSValue -> Result [Result Lesson]
extractLessons (JSArray a)  = do
  return (map handleOne a)

  where
    handleOne :: JSValue -> Result Lesson
    handleOne (JSObject o)  = do
      subject <- valFromObj subjectKey o
      day     <- valFromObj lessonDayKey o
      slot    <- valFromObj lessonSlotKey o
      return (Lesson slot day 0 subject)
    handleOne _           = Error "wrong type"

extractLessons _          = Error "wrong value type"


reportAndExecute :: Result ([Result Rule], [Result Lesson]) -> IO()
reportAndExecute (Error s)    = do
  putStrLn "Stopped execution due to a severe problem with the input data:"
  putStrLn s
reportAndExecute (Ok (r, l))  = do
  rules   <- reportOrReturn r
  lessons <- reportOrReturn l

  putStrLn "\n"
  _       <- mapM print lessons
  putStrLn "\n"

  let weighted    = weigh rules lessons

  putStrLn "\n"
  _       <- mapM print weighted
  putStrLn "\n"

  let calculated  = calc weighted

  _       <- pc calculated
  return ()
  where
    pc = mapM (\x -> putStrLn ("\n\n" ++ formatSchedule x))

    reportOrReturn :: [Result a] -> IO([a])
    reportOrReturn []     = do
      return []
    reportOrReturn (x:xs) =
      case x of
        Error s -> do
          putStrLn "Some data was unusable:"
          putStrLn s
          reportOrReturn xs
        Ok v    -> do
          ps <- reportOrReturn xs
          return (v:ps)


main :: IO()
main = do
  args <- getArgs
  let filename = head args
  json <- getFromFile filename
  let native = toNative json
  reportAndExecute native
