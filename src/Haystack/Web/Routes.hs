module Haystack.Web.Routes where

import Happstack.Server
import Haystack.Web.Website
import Haystack.Web.Preferences
import Control.Monad (msum)

routes :: ServerPart Response
routes = msum [ dir "form" $ prefPage
              , homePage
              ]
