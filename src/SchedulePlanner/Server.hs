{-# LANGUAGE OverloadedStrings   #-}
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
module SchedulePlanner.Server (server, app) where


import           Network.Wai.Handler.Warp     (run)
import           Network.Wai                  (responseLBS, lazyRequestBody,
                                              Application, requestMethod)
import           Data.ByteString.Lazy         (ByteString)
import           Network.HTTP.Types           (ok200, methodPost, methodOptions,
                                              imATeaPot418)


{-|
  The 'Application' used for the server instance.
-}
app :: (ByteString -> ByteString) -> Application
app app' request respond
  | rMethod == methodPost =
    lazyRequestBody request >>=
      respond . responseLBS ok200 headers . app'
  | rMethod == methodOptions = respond $ responseLBS ok200 headers "Bring it!"
  | otherwise = respond $ responseLBS imATeaPot418 [] "What are you doing to an innocent teapot?"
  where
    rMethod = requestMethod request
    headers = [
        ("Access-Control-Allow-Origin", "http://justus.science"),
        ("Access-Control-Allow-Methods", "POST"),
        ("Access-Control-Allow-Headers", "Content-Type")
      ]


{-|
  Run the server.
-}
server :: Int -> (ByteString -> ByteString) -> IO ()
server port = run port . app
