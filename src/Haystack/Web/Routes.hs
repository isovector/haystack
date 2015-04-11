module Haystack.Web.Routes where

import Happstack.Lite
import Haystack.Web.Website

routes :: ServerPart Response
routes = msum [ dir "form"    $ formPage
              , homePage
              ]
