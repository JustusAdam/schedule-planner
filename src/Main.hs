{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
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
  , DirectCallOptions(..)
  , CommonOptions(..)
  , SP.ServerOptions(..)
  ) where


import           Control.Applicative.Unicode
import           Control.Monad.Unicode
import           Data.ByteString.Lazy        (readFile)
import           Data.Text                   (pack)
import           Options                     (Options, defineOption,
                                              defineOptions, optionDefault,
                                              optionDescription,
                                              optionLongFlags, optionShortFlags,
                                              optionType_bool, optionType_int,
                                              optionType_list, optionType_maybe,
                                              optionType_string, runSubcommand,
                                              subcommand)
import           Prelude                     hiding (readFile)
#ifndef NOSCRAPER
import qualified SchedulePlanner             as SP (ScraperOptions (..),
                                                    ServerOptions (..),
                                                    reportAndPrint, scrape,
                                                    server, serverCalculation)
#else
import qualified SchedulePlanner             as SP (ServerOptions (..),
                                                    reportAndPrint,
                                                    server, serverCalculation)
#endif

{-|
  If no input filename is provided, use this one.
-}
stdFileName ∷ String
stdFileName = "testsuite/test.json"


-- |Temporary constant, should be in call args eventually
outputFormatDefault ∷ String
outputFormatDefault = "print"


{-|
  If no server port is provided via command line flag, this one is used.
-}
defaultServerPort ∷ Int
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
  { outputFile   ∷ Maybe String  -- ^ if provided writes the output into a file
  , inputFile    ∷ String  -- ^ the file from which to read the input, default 'stdFileName'
  , outputFormat ∷ String  -- ^ supported formats are "print" and "json", default 'outputFormatDefault'
  , verbocity    ∷ Bool    -- ^ not sure this does anything ...
  } deriving (Show)


instance Options DirectCallOptions where
  defineOptions = DirectCallOptions
    <$> defineOption
          (optionType_maybe optionType_string)
          (\o → o { optionLongFlags   = ["output-file"]
                   , optionShortFlags  = "o"
                   , optionDescription = "print output to this file instead of stdout"
                   , optionDefault     = Nothing
                   })
    ⊛ defineOption
          optionType_string
          (\o → o { optionDefault     = stdFileName
                   , optionLongFlags   = ["input-file"]
                   , optionShortFlags  = "i"
                   , optionDescription = "read input from this file"
                   })
    ⊛ defineOption
          optionType_string
          (\o → o { optionDefault     = outputFormatDefault
                   , optionLongFlags   = ["output-format"]
                   , optionShortFlags  = "f"
                   , optionDescription = "set the output format"
                   })
    ⊛ defineOption
          optionType_bool
          (\o → o { optionDefault     = False
                   , optionLongFlags   = ["verbose"]
                   , optionShortFlags  = "v"
                   , optionDescription = "Print extra information"
                   })


instance Options SP.ServerOptions where
  defineOptions = SP.ServerOptions
    <$> defineOption
          optionType_int
          (\o → o { optionLongFlags   = ["port"]
                   , optionShortFlags  = "p"
                   , optionDescription = "The port to run the server on"
                   , optionDefault     = defaultServerPort
                   })
    ⊛ defineOption
          (optionType_maybe optionType_string)
          (\o → o { optionLongFlags   = [ "logfile" ]
                   , optionShortFlags  = "l"
                   , optionDescription = "Set a logfile to enable server request logging"
                   })


#ifndef NOSCRAPER
instance Options SP.ScraperOptions where
  defineOptions = SP.ScraperOptions
    <$> defineOption
          (optionType_list ',' optionType_int)
          (\o → o { optionShortFlags = "s"
                   , optionLongFlags = ["semester"]
                   , optionDescription = "which semesters to scrape for"
                   })
    ⊛ defineOption
          (optionType_maybe optionType_string)
          (\o → o { optionLongFlags = ["output-file"]
                   , optionShortFlags = "o"
                   , optionDescription = "Save the output to this file"
                   })
#endif

{-|
  main function. Handles reading command line arguments, the json input
  and starts execution.
-}
main ∷ IO()
main =
  runSubcommand
    [ subcommand "calc" directCall
    , subcommand "serve" serverMain
#ifndef NOSCRAPER
    , subcommand "scrape" scraperMain
#endif
    ]

{-|
  Main function of the "calc" subcommand.
-}
directCall ∷ CommonOptions → DirectCallOptions → [String] → IO ()
directCall
  _
  ( DirectCallOptions
      { inputFile = ifile
      , outputFormat = outForm
      , verbocity = v
      , outputFile = outFile
      })
  _
  = readFile ifile ≫= SP.reportAndPrint (pack outForm) v outFile


{-|
  Main function of the "serve" subcommand.
-}
serverMain ∷ CommonOptions → SP.ServerOptions → [String] → IO ()
serverMain _ so _ = SP.server so SP.serverCalculation


#ifndef NOSCRAPER
scraperMain ∷ CommonOptions → SP.ScraperOptions → [String] → IO ()
scraperMain _ so [u] = SP.scrape so u
scraperMain _ _ _ = putStrLn "You need to provide a university which to scrape"
#endif
