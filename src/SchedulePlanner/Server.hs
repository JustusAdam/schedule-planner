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
                                              Application, requestHeaders)
import           Data.ByteString.Lazy         (ByteString)
import           Network.HTTP.Types           (status200, HeaderName, Header,
                                              methodPost, methodOptions)
import qualified Data.ByteString              as BS (ByteString, intercalate)


{-|
  The 'Application' used for the server instance.
-}
app :: (ByteString -> ByteString) -> Application
app app' request respond =
  lazyRequestBody request >>=
    respond . responseLBS status200 headers . app'
  where
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
