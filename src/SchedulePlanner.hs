
{-|
Module      : $Header$
Description : Collective umbrella module for the Application
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

No intrinsic use. Only harbors submodules.
-}
module SchedulePlanner
  ( reportAndPrint
  , serverCalculation
  , server
  , ServerOptions(..)
  ) where


import           SchedulePlanner.App    (reportAndPrint, serverCalculation)
import           SchedulePlanner.Server (server, ServerOptions(..))
