{-# LANGUAGE OverloadedStrings #-}

module SchedulePlanner.App (
  reportAndPrint,
  reportAndExecute
  ) where


import           Control.Monad.Writer
import           Data.Aeson                 (encode, decodeStrict)
import           Data.ByteString.Lazy       as LBS (toStrict)
import qualified Data.Map                   as Map (keys, elems)
import           Data.Text                  as T (Text, append, pack)
import qualified Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Text.IO               as TIO (putStrLn)
import           SchedulePlanner.Calculator
import           SchedulePlanner.Serialize


-- |Print a string if debug is enabled
printDebug :: Show a => Bool -> a -> Writer Text ()
printDebug debugMode = when debugMode . tell . pack . show


{-|
  Evaluates the transformed json, compiles (useful) error messages, prints them
  and then runs the algorithm or, if the errors are too severe, aborts.
-}
reportAndExecute :: Text -> Bool -> Maybe DataFile -> Writer Text ()
reportAndExecute _ _ (Nothing)    =
  tell "Stopped execution due to a severe problem with the input data:"
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
          tell $ Data.Text.Encoding.decodeUtf8 $ toStrict $ encode $ concatMap Map.elems calculated
          return ()

        _ -> tell "invalid output format"

  where
    pc = mapM (tell . append "\n\n" . formatSchedule)


reportAndPrint :: Text -> Bool -> Text -> IO()
reportAndPrint outputFormat debugMode rawInput =
  TIO.putStrLn $ snd $ runWriter $ reportAndExecute outputFormat debugMode $ decodeStrict $ Data.Text.Encoding.encodeUtf8 rawInput
