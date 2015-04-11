
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Haystack.Web.Preferences where

import Haystack.Game
import Control.Applicative ((<$>))
import Control.Monad (forM_, mapM_)
import Data.Char (toLower)
import Data.String.Utils (replace)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, option, select)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Haystack.Web.Template



prefTable :: Html
prefTable = H.table $ forM_ [ "Popular"
                            , "Forgotten Gems"
                            , "New Releases"
                            , "Family Games"
                            , "Party Games"
                            , "Abstract Strategy Games"
                            , "2 Player Games"
                            , "3+ Player Games"
                            ] makeRow
  where makeRow s = prefRow s (H.stringValue . map toLower . replace " " "-" $ s)
        prefRow s v = H.tr $ do
                        H.td ! A.style "text-align: right" $ label $ H.string (s ++ ":")
                        H.td                               $ mapM_ prefRadio [1..5]
          where prefRadio i = input
                            ! type_ "radio"
                            ! name v
                            ! value (H.stringValue $ show i)


prefPage :: ServerPart Response
prefPage = msum [ viewForm, processForm ]
  where
      viewForm :: ServerPart Response
      viewForm =
          do method GET
             ok $ template "form" $
               form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
                   prefTable
                   label ! A.for "msg" $ "Say something clever"
                   input ! type_ "text" ! A.id "msg" ! name "msg"
                   input ! type_ "submit" ! value "Say it!"

      processForm :: ServerPart Response
      processForm =
          do method POST
             cat :: Category <- read . unpack <$> lookText "category"
             msg <- lookText "msg"
             ok $ template "form" $ do
                 H.p "You said:"
                 H.p (toHtml msg)
                 H.p (toHtml $ show cat)

