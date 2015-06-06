{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
Description : Connector from IO to logic
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Sort of the Main script for all the common operations, independant of the
program instance (webservice, command line)
-}
module SchedulePlanner.App
  ( reportAndPrint
  , reportAndExecute
  , serverCalculation
  ) where


import           Control.Monad.Writer       (Writer, runWriter, tell, when)
import           Data.Aeson                 (eitherDecode, encode)
import           Data.ByteString.Lazy       as LBS (toStrict, ByteString)
import qualified Data.Map                   as Map (elems, keys)
import           Data.Text                  as T (Text, append, pack)
import qualified Data.Text.Encoding         (decodeUtf8)
import           Data.Text.IO               as TIO (putStrLn)
import           SchedulePlanner.Calculator (calcFromMap, mapToSubject, weigh, MappedSchedule)
import           SchedulePlanner.Serialize  (DataFile (DataFile),
                                             formatSchedule, shortSubject, scheduleToJson)
import           Data.String (fromString)


-- |Print a string if debug is enabled
printDebug :: Show a => Bool -> a -> Writer Text ()
printDebug debugMode = when debugMode . tell . pack . show


calculate :: DataFile -> Maybe [MappedSchedule Text]
calculate (DataFile rules lessons) =
  let
    weighted      = weigh rules lessons
    mappedLessons = mapToSubject weighted
  in calcFromMap mappedLessons


serverCalculation :: ByteString -> ByteString
serverCalculation =
  either
    (fromString . ("Error:" ++) . show )
    (maybe
      "\"No schedule could be calculated\""
      (encode . map scheduleToJson)
    . calculate)
  . eitherDecode


{-|
  Evaluates the transformed json, compiles (useful) error messages, runs the algorithm
  and returns a writer of any output created.
-}
reportAndExecute :: Text -> Bool -> DataFile -> Writer Text ()
reportAndExecute outputFormat debugMode (DataFile rules lessons)  = do
  let weighted      = weigh rules lessons

  let mappedLessons = mapToSubject weighted

  maybe
    (tell "Calculation failed, no valid schedule possible")

    (\calculated ->

      case outputFormat of

        "print" -> do

          tell "\n"
          _       <- mapM (printDebug debugMode) rules
          tell "\n"

          tell "\n"
          _       <- mapM (printDebug debugMode) weighted
          tell "\n"

          tell "Legend:"
          _       <- mapM (tell . pack . show . (pure (,) <*> shortSubject <*> id) ) (Map.keys mappedLessons)


          tell "\n"
          _       <- pc calculated
          return ()

        "json" -> do
          tell $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode $ concatMap Map.elems calculated
          return ()

        _ -> tell "invalid output format")

    (calcFromMap mappedLessons)

  where
    pc = mapM (tell . append "\n\n" . formatSchedule)


{-|
  perform the calculation and print the result to the command line
-}
reportAndPrint :: Text -> Bool -> ByteString -> IO()
reportAndPrint outputFormat debugMode =
  TIO.putStrLn . either
    (pack . ("Stopped execution due to a severe problem with the input data:" ++) . show)
    (snd . runWriter . reportAndExecute outputFormat debugMode)
     . eitherDecode
