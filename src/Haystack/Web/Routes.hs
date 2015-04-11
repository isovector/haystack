module Haystack.Web.Routes where

import Happstack.Server
import Haystack.Database
import Haystack.Web.Website
import Haystack.Web.Preferences
import Control.Monad (msum)

routes :: App Response
routes = msum [ dir "form" $ prefPage
              , homePage
              ]
