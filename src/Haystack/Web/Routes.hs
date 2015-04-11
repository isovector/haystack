module Haystack.Web.Routes where

import Happstack.Lite
import Haystack.Web.Website
import Haystack.Web.Preferences

routes :: ServerPart Response
routes = msum [ dir "form" $ prefPage
              , homePage
              ]
