
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Haystack.Web.Preferences where

import Haystack.Game
import Control.Applicative ((<$>))
import Control.Monad (forM_, mapM_)
import Data.Text (Text)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, option, select)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Haystack.Web.Template

prefMap :: [(String, String)]
prefMap = [ ("popular", "Popular")
          , ("gems", "Forgotten Gems")
          , ("new", "New Releases")
          , ("family", "Family Games")
          , ("party", "Party Games")
          , ("abstract", "Abstract Games")
          , ("strategy", "Themed Strategy Games")
          , ("player2", "2 Player Games")
          , ("player3", "3+ Player Games")
          ]


noPrefTable :: Html
noPrefTable = forM_ prefMap noPrefRow
  where noPrefRow (v, _) = input
                         ! type_ "hidden"
                         ! name (H.stringValue v)
                         ! value "3"

prefTable :: Html
prefTable = H.table $ forM_ prefMap prefRow
  where
      prefRow (v, s) = H.tr $ do
          H.td ! A.style "text-align: right" $ label $ H.string (s ++ ":")
          H.td                               $ mapM_ prefRadio [1..5]
        where prefRadio i = input
                          ! type_ "radio"
                          ! name (H.stringValue v)
                          ! value (H.stringValue $ show i)

runForm :: H.AttributeValue -> Html -> Html
runForm submit contents =
    form ! action "/form"
         ! enctype "multipart/form-data"
         ! A.method "POST" $ do
             contents
             input ! type_ "submit" ! value submit



prefPage :: ServerPart Response
prefPage = msum [ viewForm, processForm ]
  where
      viewForm =
          do method GET
             ok $ template "form" $ do
               runForm "No Preferences" noPrefTable
               runForm "Submit!" prefTable

      processForm =
          do method POST
             prefs <- sequence $ map (runText . fst) prefMap
             ok $ template "form" $ do
                 H.p "You said:"
                 forM_ prefs output

      runText idx = do rating <- lookText idx
                       return (idx, rating)

      output (idx, val) = do H.p $ toHtml idx
                             H.p $ toHtml $ show val
