
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Haystack.Web.Preferences where

import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace)
import Haystack.Game
import Haystack.Database
import Control.Applicative ((<$>), optional)
import Control.Monad (forM_, mapM_, msum)
import Control.Monad.Reader (ask, ReaderT)
import Data.Acid
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server
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


prefTable :: Html
prefTable = H.table $ forM_ prefMap prefRow
  where
      prefRow (v, s) = H.tr $ do
          H.td ! A.style "text-align: right" $ label $ H.string (s ++ ":")
          H.td                               $ mapM_ prefRadio [0..4]
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


prefPage :: App Response
prefPage = msum [ viewForm, processForm ]
  where
      viewForm =
          do method GET
             ok $ template "form" $ do
               runForm "No Preferences" ""
               runForm "Submit!" prefTable

      processForm =
          do method POST
             db <- ask
             (games, _) <- liftIO $ query db (GetState)
             formData <- sequence $ map (runText . fst) prefMap
             let prefs = buildPref formData

             ok $ template "form" $ do
                 H.p "You said:"
                 printIt prefs
                 mapM_ printIt games

      buildPref prefs =
          GamePref { likesPopular    = get "popular"
                   , likesNewRelease = get "new"
                   , likesForgotten  = get "gems"
                   , likesFamily     = get "family"
                   , likesParty      = get "party"
                   , likesAbstract   = get "abstract"
                   , likesStrategy   = get "strategy"
                   , likes2Player    = get "player2"
                   , likes3Player    = get "player3"
                   }
        where get x = fromMaybe 2 $ lookup x prefs

      runText idx = do formVal <- optional $ lookText idx
                       let rating = fmap (read . unpack) formVal
                       return (idx, fromMaybe 2 rating)

      printIt game = H.p $ toHtml (show game)
