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
import           Network.Wai.Middleware.Cors  (cors, CorsResourcePolicy(..),
                                              simpleCorsResourcePolicy)
import           Data.ByteString.Lazy         (ByteString)
import           Network.HTTP.Types           (status200, HeaderName, Header,
                                              methodPost, methodOptions)
import qualified Data.ByteString              as BS (ByteString, intercalate)
import           Debug.Trace                  (traceShow)


{-|
  CORS application policy for the AJAX requests.
-}
applicationPolicy :: CorsResourcePolicy
applicationPolicy = simpleCorsResourcePolicy
  { corsOrigins = Just ([ "http://justus.science", "http://justus.science:80" ], False)
  , corsMethods = [ methodPost ]
  }


{-|
  The 'Application' used for the server instance.
-}
app :: (ByteString -> ByteString) -> Application
app app' request respond =
  lazyRequestBody request >>=
    respond . responseLBS status200 headers . app'
  where
    showIt :: Show a => a -> a
    showIt a = traceShow a a

    headers = []


{-|
  Run the server.
-}
server :: Int -> (ByteString -> ByteString) -> IO ()
server port = run port . cors (const $ Just applicationPolicy) . app
