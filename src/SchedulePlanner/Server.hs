{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-|
Module      : $Header$
Description : functions necessary for deploying the application as a webservice
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Uses happstack and blaze to create a deployable service instance of this software.
-}
module SchedulePlanner.Server (server, app, ServerOptions(..)) where


import           Data.ByteString.Lazy     (ByteString)
import           Data.Composition         ((.:))
import           Network.HTTP.Types       (imATeaPot418, methodOptions,
                                           methodPost, ok200)
import           Network.Wai              (Application, lazyRequestBody,
                                           requestMethod, responseLBS, remoteHost)
import           Network.Wai.Handler.Warp (run)
#ifndef NOSCRAPER
import           SchedulePlanner.Scraper
#endif
import           System.IO                (IOMode(AppendMode), withFile, hPutStrLn)


{-|
  Options used for the "serve" subcommand.
-}
data ServerOptions = ServerOptions
  { port    :: Int -- ^ default 'defaultServerPort'
  , logFile :: Maybe FilePath
  }


{-|
  The 'Application' used for the server instance.
-}
app :: ServerOptions -> (ByteString -> ByteString) -> Application
app opts app' request respond
  | rMethod == methodPost =
    logPureReq ("New POST request from " ++ (show $ remoteHost request)) >>
    lazyRequestBody request >>=
      respond . responseLBS ok200 headers . app'
  | rMethod == methodOptions = respond $ responseLBS ok200 headers "Bring it!"
  | otherwise =
    logPureReq ("Unhandleable request: " ++ show request) >>
    respond (responseLBS imATeaPot418 [] "What are you doing to an innocent teapot?")
  where
    logAction logfile = withFile logfile AppendMode . flip hPutStrLn
    logPureReq message =
      maybe
        (return ())
        (flip logAction message)
        (logFile opts)
    logIOReq messageGetter =
      maybe
        (return ())
        ((>>=) messageGetter . logAction)
        (logFile opts)
    rMethod = requestMethod request
    headers = [
        ("Access-Control-Allow-Origin", "http://justus.science"),
        ("Access-Control-Allow-Methods", "POST"),
        ("Access-Control-Allow-Headers", "Content-Type")
      ]


{-|
  Run the server.
-}
server :: ServerOptions -> (ByteString -> ByteString) -> IO ()
server opts@(ServerOptions { port = port }) = run port . app opts
