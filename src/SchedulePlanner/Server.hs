{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SchedulePlanner.Server () where

import           Control.Applicative         (optional, (<$>))
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import           Data.Text.Lazy              (unpack)
import           Happstack.Lite
import           Text.Blaze.Html5            (Html, a, form, input, label, p,
                                              toHtml, (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size,
                                              type_, value)
import qualified Text.Blaze.Html5.Attributes as A
