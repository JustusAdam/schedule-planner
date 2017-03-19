module SchedulePlanner.Util where


import           System.IO as SIO
import ClassyPrelude


logLine :: String -> IO ()
logLine = SIO.hPutStrLn SIO.stderr
