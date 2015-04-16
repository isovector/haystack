{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Haystack.Web.Preferences where

import Prelude hiding (forM_, mapM_)

import Debug.Trace (trace)
import Control.Applicative ((<$>), optional)
import Control.Monad (msum)
import Control.Monad.Reader (ask, ReaderT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, option, select)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Haystack
import Haystack.Web.Template


prefMap :: [(String, String)]
prefMap = [ ("popular",  "Popular")
          , ("gems",     "Forgotten Gems")
          , ("new",      "New Releases")
          , ("family",   "Family Games")
          , ("party",    "Party Games")
          , ("abstract", "Abstract Games")
          , ("strategy", "Themed Strategy Games")
          , ("player2",  "2 Player Games")
          , ("player3",  "3+ Player Games")
          ]


prefTable :: Html
prefTable = H.table $ do H.tr $ do H.td ! A.style "text-align: right"
                                        $ label "Username:"
                                   H.td $ input
                                        ! type_ "input"
                                        ! name "username"
                         forM_ prefMap prefRow
  where
      prefRow (v, s) = H.tr $ do
          H.td ! A.style "text-align: right" $ label $ H.string (s ++ ":")
          H.td                               $ do "hate"
                                                  mapM_ prefRadio [0..4]
                                                  "love"
        where prefRadio i = input
                          ! type_ "radio"
                          ! name (H.stringValue v)
                          ! value (H.stringValue $ show i)

runForm :: H.AttributeValue -> Html -> Html
runForm submit contents =
    form ! action "/form"
         ! enctype "multipart/form-data"
         ! A.method "POST" $ do
             input ! type_ "submit" ! value "No preferences!"
             contents
             input ! type_ "submit" ! value submit


prefPage :: App Response
prefPage = msum [ viewForm, processForm ]
  where
      viewForm =
          do method GET
             ok $ template "Update Preferences" $ do
               runForm "Submit!" prefTable

      processForm =
          do method POST
             db <- ask

             username <- unpack <$> lookText "username"
             formData <- sequence $ map (runText . fst) prefMap
             let prefs = buildPref formData

             liftIO $ update db (SetPrefs username prefs)

             ok $ template "Preferences Saved" $ do
                 H.p "Your preferences have been saved!"

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

      printScores pref game =
          H.p $ do
              toHtml $ gameName game
              H.br
              toHtml (show . score pref $ metadata game)


