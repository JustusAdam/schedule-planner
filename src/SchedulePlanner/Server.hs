{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


import           Network.Wai.Handler.Warp (run)
import           Network.Wai              (responseLBS, lazyRequestBody, Application, requestHeaders)
import           Data.ByteString.Lazy     (ByteString)
import           Network.HTTP.Types       (status200, HeaderName, Header)
import qualified Data.ByteString          as BS (ByteString, intercalate)


hOrigin :: HeaderName
hOrigin = "Origin"


hCrossOrigin :: HeaderName
hCrossOrigin = "Access-Control-Allow-Origin"


hAccessMethods :: HeaderName
hAccessMethods = "Access-Control-Allow-Methods"


crossOriginHeaderField :: [BS.ByteString] -> Header
crossOriginHeaderField = (,) hCrossOrigin . BS.intercalate ", "


app :: (ByteString -> ByteString) -> Application
app app' request respond = do
  body <- lazyRequestBody request
  respond $ responseLBS status200 headers $ app' body
  where
    rheaders = requestHeaders request
    headers =
      if elem hOrigin (map fst rheaders)
        then [ crossOriginHeaderField [ "http://justus.science" ], (hAccessMethods, "POST") ]
        else []

server :: Int -> (ByteString -> ByteString) -> IO ()
server port = run port . app
