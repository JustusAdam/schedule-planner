{-# LANGUAGE OverloadedStrings #-}
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
module Main (main) where


import           Data.Text                  (pack)
import           Data.ByteString.Lazy       (readFile)
import           Options                    (Options, defineOption,
                                             defineOptions, optionDefault,
                                             optionDescription, optionLongFlags,
                                             optionShortFlags, optionType_bool,
                                             optionType_maybe, optionType_int,
                                             optionType_string, runSubcommand,
                                             subcommand)
import           Prelude                    hiding (readFile)
import           SchedulePlanner.App        (reportAndPrint, serverCalculation)
import qualified SchedulePlanner.Server     as Server (server)


{-|
  If no input filename is provided, use this one.
-}
stdFileName :: String
stdFileName = "testsuite/test.json"


-- |Temporary constant, should be in call args eventually
outputFormatDefault :: String
outputFormatDefault = "print"


{-|
  If no server port is provided via command line flag, this one is used.
-}
defaultServerPort :: Int
defaultServerPort = 7097


{-|
  Common options among all subcommands.
-}
data CommonOptions = CommonOptions


instance Options CommonOptions where
  defineOptions = pure CommonOptions


{-|
  Command line options for the "calc" subcommand.
-}
data DirectCallOptions = DirectCallOptions
  { outputFile   :: Maybe String -- ^ if provided writes the output into a file
  , inputFile    :: String -- ^ the file from which to read the input, default 'stdFileName'
  , outputFormat :: String -- ^ supported formats are "print" and "json", default 'outputFormatDefault'
  , verbocity    :: Bool   -- ^ not sure this does anything ...
  } deriving (Show)


instance Options DirectCallOptions where
  defineOptions = DirectCallOptions
    <$> defineOption
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
                   , optionLongFlags   = ["verbose"]
                   , optionShortFlags  = "v"
                   , optionDescription = "Print extra information"
                   })


{-|
  Options used for the "serve" subcommand.
-}
data ServerOptions = ServerOptions
  { port :: Int -- ^ default 'defaultServerPort'
  }


instance Options ServerOptions where
  defineOptions = ServerOptions
    <$> defineOption
          optionType_int
          (\o -> o { optionLongFlags   = ["port"]
                   , optionShortFlags  = "p"
                   , optionDescription = "The port to run the server on"
                   , optionDefault     = defaultServerPort
                   })


{-|
  main function. Handles reading command line arguments, the json input
  and starts execution.
-}
main :: IO()
main =
  runSubcommand
    [ subcommand "calc" directCall
    , subcommand "serve" serverMain
    ]

{-|
  Main function of the "calc" subcommand.
-}
directCall :: CommonOptions -> DirectCallOptions -> [String] -> IO ()
directCall
  _
  ( DirectCallOptions
      { inputFile = ifile
      , outputFormat = outForm
      , verbocity = v
      })
  _
  = readFile ifile >>=
        reportAndPrint (pack outForm) v


{-|
  Main function of the "serve" subcommand.
-}
serverMain :: CommonOptions -> ServerOptions -> [String] -> IO ()
serverMain _ (ServerOptions { port = p }) _ = Server.server p serverCalculation
