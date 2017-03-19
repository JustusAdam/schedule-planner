{-# LANGUAGE FlexibleContexts #-}
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


import           Control.Arrow              ((&&&))
import           Control.Monad              (void)
import           Control.Monad.Writer       (Writer, runWriter, tell, when)
import           Data.Aeson                 (eitherDecode, encode)
import           Data.ByteString.Lazy       as LBS (ByteString, toStrict)
import qualified Data.Map                   as Map (elems, keys)
import           Data.Maybe                 (isNothing)
import           Data.Monoid.Unicode
import           Data.String                (fromString)
import           Prelude.Unicode
import           SchedulePlanner.Calculator (MappedLessons (..),
                                             MappedSchedule (..), calcFromMap,
                                             mapToSubject, weigh)
import           SchedulePlanner.Serialize  (DataFile (DataFile),
                                             formatSchedule, scheduleToJson,
                                             shortSubject)
import ClassyPrelude

-- |Print a string if debug is enabled
printDebug :: (MonadWriter Text m, Show a) => Bool -> a -> m Text ()
printDebug debugMode = when debugMode . tell . pack . show


{-|
  Calculation on internal data structures.
-}
calculate :: DataFile -> Maybe [MappedSchedule Text]
calculate (DataFile rules lessons) =
  calcFromMap $ mapToSubject $ weigh rules lessons


{-|
  Calculation wrapped into server I/O compatible data structures.
-}
serverCalculation :: ByteString -> ByteString
serverCalculation =
  either
    (fromString . ("Error:" ++) . show)
    (maybe
      "\"No schedule could be calculated\""
      (encode . map scheduleToJson)
    . calculate)
  . eitherDecode


{-|
  Evaluates the transformed json, compiles (useful) error messages, runs the algorithm
  and returns a writer of any output created.
-}
reportAndExecute :: MonadWriter Text m => Text -> Bool -> DataFile -> m Text ()
reportAndExecute outputFormat debugMode (DataFile rules lessons)
  | isNothing calculated = tell "Calculation failed, no valid schedule possible"
  | outputFormat' == "print" = do
    tell "\n"
    _       <- mapM (printDebug debugMode) rules
    tell "\n"

    tell "\n"
    _       <- mapM (printDebug debugMode) weighted
    tell "\n"

    tell "Legend:"
    _       <- mapM (tell . pack . show . (shortSubject &&& id) ) (Map.keys mlRaw)


    tell "\n"
    void $ maybe (error "Unexpected missing result") pc calculated
  | outputFormat' == "json" =
    void $ maybe (error "unexpected missing result") (tell . decodeUtf8 . toStrict . encode . concatMap (Map.elems . unMapSchedule)) calculated


  | otherwise = tell "invalid output format"

  where
    outputFormat' = toLower outputFormat
    weighted      = weigh rules lessons
    mappedLessons@(MappedLessons mlRaw) = mapToSubject weighted
    pc            = mapM (tell . ("\n\n" ⊕) . formatSchedule)
    calculated    = calcFromMap mappedLessons


{-|
  perform the calculation and print the result to the command line
-}
reportAndPrint :: Text -> Bool -> Maybe String -> ByteString -> IO()
reportAndPrint outputFormat debugMode outFile =
  maybe putStrLn writeFile outFile . either
    (pack . ("Stopped execution due to a severe problem with the input data:" ++) . show)
    (snd . runWriter . reportAndExecute outputFormat debugMode)
     . eitherDecode
