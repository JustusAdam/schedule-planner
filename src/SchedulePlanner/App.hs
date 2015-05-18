{-# LANGUAGE OverloadedStrings #-}

module SchedulePlanner.App (
  reportAndPrint,
  reportAndExecute
  ) where


import           Control.Applicative        (pure, (<*>))
import           Control.Monad.Writer
import           Data.Aeson                 (ToJSON, decode, encode)
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.List                  as List (take)
import qualified Data.Map                   as Map (keys)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  as T (Text, append, pack)
import qualified Data.Text.Encoding         (decodeUtf8)
import           Data.Text.IO               as TIO (hPutStrLn)
import           SchedulePlanner.Calculator
import           SchedulePlanner.Serialize
import           System.IO                  (hPutStrLn, stderr)



-- |Print a line to stdout
putErrorLine :: Text -> IO()
putErrorLine = TIO.hPutStrLn stderr


-- |Print a string if debug is enabled
printDebug :: Show a => Bool -> a -> Writer Text ()
printDebug debugMode = when debugMode . tell . pack . show


{-|
  Evaluates the transformed json, compiles (useful) error messages, prints them
  and then runs the algorithm or, if the errors are too severe, aborts.
-}
reportAndExecute :: (Show a, Ord a, ToJSON a) => Text -> Bool -> Maybe (DataFile a) -> Writer Text ()
reportAndExecute _ _ (Nothing)    =
  tell $ "Stopped execution due to a severe problem with the input data:"
reportAndExecute outputFormat debugMode (Just (DataFile rules lessons))  = do
  let weighted      = weigh rules lessons

  let mappedLessons = mapToSubject weighted

  let result        = calcFromMap mappedLessons

  case result of
    Nothing ->
      tell "Calculation failed, no valid schedule possible"
    Just calculated ->

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
          tell $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode calculated
          return ()

  where
    pc = mapM (tell . append "\n\n" . formatSchedule)


reportAndPrint :: Text -> Bool -> Text -> IO()
reportAndPrint outputFormat debugMode rawInput =
  putStrLn $ snd $ runWriter $ reportAndExecute outputFormat debugMode (decode rawInput)
