{-# LANGUAGE UnicodeSyntax #-}
module SchedulePlanner.Util where


import           System.IO as SIO


logLine ∷ String → IO ()
logLine = SIO.hPutStrLn SIO.stderr
