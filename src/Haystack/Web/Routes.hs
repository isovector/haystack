module Haystack.Web.Routes where

import Happstack.Server
import Haystack.Database
import Haystack.Web.Preferences
import Control.Monad (msum)

routes :: App Response
routes = msum [ dirs "api/preferences" $ servePrefs
              , prefPage
              ]
