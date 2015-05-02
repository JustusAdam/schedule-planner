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

import           Control.Applicative        (pure, (<*>))
import           Control.Monad              (liftM, when)
import qualified Data.List                  as List (take)
import qualified Data.Map                   as Map (keys)
import           Data.Maybe                 (fromMaybe)
import           Options                    (Options, defineOption,
                                             defineOptions, optionDefault,
                                             optionDescription, optionLongFlags,
                                             optionShortFlags, optionType_maybe,
                                             optionType_string, runCommand)
import           SchedulePlanner.Calculator
import           SchedulePlanner.Serialize
import           System.IO                  (hPutStrLn, stderr)
import           Text.JSON                  as JSON (Result (..))


-- |Enables debug messages
debugMode     = True
-- |Temporary constant, should be in call args eventually
outputFormatDefault = "print"


-- |Legacy hard coded name of inputfile
stdFileName   = "testsuite/test.json"


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


-- |Print a line to stdout
putErrorLine :: String -> IO()
putErrorLine = hPutStrLn stderr


-- |Print a string if debug is enabled
printDebug :: Show a => a -> IO()
printDebug = when debugMode . print


{-|
  Evaluates the transformed json, compiles (useful) error messages, prints them
  and then runs the algorithm or, if the errors are too severe, abourts.
-}
reportAndExecute :: String -> Result ([Result Rule], [Result (Lesson String)]) -> IO()
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
          _       <- mapM (print . (pure (,) <*> List.take 10 <*> id) ) (Map.keys mappedLessons)


          putStrLn "\n"
          _       <- pc calculated
          return ()

        "json" -> do
          print $  serialize calculated
          return ()


  where
    pc = mapM (putStrLn.("\n\n" ++).formatSchedule)

    reportOrReturn :: [Result a] -> IO [a]
    reportOrReturn []     = return []
    reportOrReturn (x:xs) =
      case x of
        Error s -> do
          putErrorLine "Some data was unusable:"
          putErrorLine s
          reportOrReturn xs
        Ok v    -> liftM (v:) (reportOrReturn xs)


{-|
  main function. Handles reading command line arguments, the json input
  and starts execution.
-}
main :: IO()
main = runCommand $ \opts args -> do
  rawInput <- getFromFile $ inputFile opts

  reportAndExecute (outputFormat opts) (deSerialize rawInput)
