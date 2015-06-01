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


import Network.Wai.Handler.Warp (run)
import Network.Wai (responseLBS, lazyRequestBody, Application)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (status200)


app :: (ByteString -> ByteString) -> Application
app a request respond = do
  body <- lazyRequestBody request
  respond $ responseLBS status200 [] $ a body

server :: Int -> (ByteString -> ByteString) -> IO ()
server port = run port . app
