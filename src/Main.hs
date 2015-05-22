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
module Main
  ( main
  , CallOptions
  ) where

import           Data.Text                  (pack)
import           Data.Text.IO               (readFile)
import           Options                    (Options, defineOption,
                                             defineOptions, optionDefault,
                                             optionDescription, optionLongFlags,
                                             optionShortFlags, optionType_bool,
                                             optionType_maybe,
                                             optionType_string, runCommand)
import           Prelude                    hiding (readFile)
import           SchedulePlanner.App        (reportAndPrint)


stdFileName :: String
stdFileName = "testsuite/test.json"


-- |Temporary constant, should be in call args eventually
outputFormatDefault :: String
outputFormatDefault = "print"


data CallOptions = CallOptions { outputFile   :: Maybe String
                               , inputFile    :: String
                               , outputFormat :: String
                               , server       :: Bool
                               , verbocity    :: Bool
                               } deriving (Show)

instance Options CallOptions where
  defineOptions = pure CallOptions
    <*> defineOption
          (optionType_maybe optionType_string)
          (\o -> o { optionLongFlags   = ["output-file"]
                   , optionShortFlags  = "o"
                   , optionDescription = "print output to this file instead of stdout"
                   , optionDefault     = Nothing
                   })
    <*> defineOption
          optionType_string
          (\o -> o { optionDefault     = stdFileName
                   , optionLongFlags   = ["input-file"]
                   , optionShortFlags  = "i"
                   , optionDescription = "read input from this file"
                   })
    <*> defineOption
          optionType_string
          (\o -> o { optionDefault     = outputFormatDefault
                   , optionLongFlags   = ["output-format"]
                   , optionShortFlags  = "f"
                   , optionDescription = "set the output format"
                   })
    <*> defineOption
          optionType_bool
          (\o -> o { optionDefault     = False
                   , optionLongFlags   = ["serve"]
                   , optionDescription = "Placeholder with no effect yet"
                   })
    <*> defineOption
          optionType_bool
          (\o -> o { optionDefault     = False
                   , optionLongFlags   = ["verbose"]
                   , optionShortFlags  = "v"
                   , optionDescription = "Print extra information"
                   })


{-|
  main function. Handles reading command line arguments, the json input
  and starts execution.
-}
main :: IO()
main = runCommand $ \opts args ->
  readFile (inputFile opts) >>=
  reportAndPrint (pack $ outputFormat opts) (verbocity opts)
