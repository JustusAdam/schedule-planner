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

import           Calculator.Solver
import Calculator.Scale
import Text.JSON as JSON


stdFileName = "../testsuite/test.json"
ruleKey     = "rules"
lessonKey   = "lessons"


getFromFile :: JSON a => String -> IO(Result a)
getFromFile filename = do
  string <- readFile filename
  return $ decodeStrict string


toNative :: Result JSValue -> Result ([Rule], [Lesson])
toNative i = do
  v <- i
  inner v

  where
    inner :: JSValue -> Result ([Rule], [Lesson])
    inner JSObject o  = do
      rv      <- valFromObj ruleKey o
      lv      <- valFromObj lessonKey o

      rules   <- extractRules rv
      lessons <- extractLessons lv

      return (rules, lessons)
    inner _           = Error ("wrong value type")


extractRules :: JSValue -> Result [Result Rule]
extractRules JSArray a  = do
  rv <- a
  return handleOne rv

  where
    handleOne :: JSValue -> Result Rule
    handleOne JSObject o  = do
      scope     <- valFromObj "scope" o
      severity  <- valFromObj "severity" o

      let rp = \x -> Rule x severity


      day       <- valFromObj "day" o
      slot      <- valFromObj "slot" o

      case scope of
        "day" ->
          return rp $ Day day
        "slot" ->
          return rp $ Slot slot
        "cell" ->
          return rp $ Cell day slot

    handleOne _           = Error "wrong value type"

extractRules _          = Error "wrong value type"


extractLessons :: JSValue -> Result [Result Lesson]
extractLessons JSArray a  = do
  lv <- a
  return handleOne lv

  where
    handleOne :: JSValue -> Result Lesson
    handleOne JSObject o  = do
      subject <- valFromObj "subject" o
      day     <- valFromObj "day" o
      slot    <- valFromObj "slot" o
      return Lesson slot day 0 subject
    handleOne _           = Error "wrong type"

extractLessons _          = Error "wrong value type"


main :: IO()
main = do
  let lessons = [
          Lesson 1 1 2 "TGI",
          Lesson 1 1 2 "FS",
          Lesson 1 1 1 "TGI",
          Lesson 2 2 3 "TGI",
          Lesson 1 1 3 "RA",
          Lesson 2 2 2 "RA",
          Lesson 1 2 3 "FS"
        ]
  putStrLn "\n"
  _ <- mapM print lessons
  putStrLn "\n"
  let calculated = calc lessons

  _ <- pc calculated
  return ()

  where
    pc = mapM (\x -> putStrLn ("\n\n" ++ formatSchedule x))
