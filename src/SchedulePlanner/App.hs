module SchedulePlanner.App (
  reportAndPrint,
  reportAndExecute
  ) where


import           Control.Applicative        (pure, (<*>))
import           Control.Monad.Writer
import qualified Data.List                  as List (take)
import qualified Data.Map                   as Map (keys)
import           Data.Maybe                 (fromMaybe)
import           SchedulePlanner.Calculator
import           SchedulePlanner.Serialize
import           System.IO                  (hPutStrLn, stderr)
import           Text.JSON                  as JSON (Result (..))



-- |Print a line to stdout
putErrorLine :: String -> IO()
putErrorLine = hPutStrLn stderr


-- |Print a string if debug is enabled
printDebug :: Show a => Bool -> a -> Writer String ()
printDebug debugMode = when debugMode . tell . show


{-|
  Evaluates the transformed json, compiles (useful) error messages, prints them
  and then runs the algorithm or, if the errors are too severe, aborts.
-}
reportAndExecute :: String -> Bool -> Result ([Result Rule], [Result (Lesson String)]) -> Writer String ()
reportAndExecute _ _ (Error s)    =
  tell $ "Stopped execution due to a severe problem with the input data:" ++ show s
reportAndExecute outputFormat debugMode (Ok (r, l))  = do
  rules   <- reportOrReturn r
  lessons <- reportOrReturn l

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
          _       <- mapM (tell . show . (pure (,) <*> shortSubject <*> id) ) (Map.keys mappedLessons)


          tell "\n"
          _       <- pc calculated
          return ()

        "json" -> do
          tell $  serialize calculated
          return ()

  where
    pc = mapM (tell.("\n\n" ++).formatSchedule)

    reportOrReturn :: [Result a] -> Writer String [a]
    reportOrReturn []     = return []
    reportOrReturn (x:xs) =
      case x of
        Error s -> do
          tell "Some data was unusable:"
          tell s
          reportOrReturn xs
        Ok v    -> liftM (v:) (reportOrReturn xs)


reportAndPrint :: String -> Bool -> String -> IO()
reportAndPrint outputFormat debugMode rawInput =
  putStrLn $ snd $ runWriter $ reportAndExecute outputFormat debugMode (deSerialize rawInput)
