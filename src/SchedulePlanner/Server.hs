{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-|
Module      : $Header$
Description : functions necessary for deploying the application as a webservice
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

Uses wai and warp to create a deployable web service instance of this software.
-}
module SchedulePlanner.Server (server, app, ServerOptions(..)) where


import           Control.Monad.Unicode
import           Data.ByteString.Lazy     (ByteString)
import           Network.HTTP.Types       (Header, imATeaPot418, methodOptions,
                                           methodPost, ok200)
import           Network.Wai              (Application, lazyRequestBody,
                                           remoteHost, requestMethod,
                                           responseLBS)
import           Network.Wai.Handler.Warp (run)
import           Prelude.Unicode
import           SchedulePlanner.Util


defaultHeaders :: [Header]
defaultHeaders =
  [ ("Access-Control-Allow-Origin" , "http://justus.science")
  , ("Access-Control-Allow-Methods", "POST")
  , ("Access-Control-Allow-Headers", "Content-Type")
  ]


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
app _ app' request respond
  | rMethod == methodPost =
    logLine ("New POST request from " ++ show (remoteHost request)) ≫
    lazyRequestBody request ≫=
      respond . responseLBS ok200 headers . app'
  | rMethod == methodOptions = respond $ responseLBS ok200 headers "Bring it!"
  | otherwise =
    logLine ("Unhandleable request: " ++ show request) ≫
    respond (responseLBS imATeaPot418 [] "What are you doing to an innocent teapot?")
  where
    rMethod = requestMethod request
    headers = defaultHeaders


{-|
  Run the server.
-}
server :: ServerOptions -> (ByteString -> ByteString) -> IO ()
server opts@(ServerOptions { port, logFile }) =
  (≫) serverInit . run port . app opts
  where
    serverInit = do
      logLine $ "Server starting on port " ++ show port
      logLine $ "Logging: " ++ maybe "disabled" ((++) "enbled, logging to " . show) logFile
