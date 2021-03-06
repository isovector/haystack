{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Haystack.Web.Preferences where

import Prelude hiding (forM_, mapM_)

import Control.Applicative ((<$>), optional)
import Control.Monad (msum, liftM2)
import Control.Monad.Reader (ask, ReaderT)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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

row :: String -> Html -> Html
row rowlabel contents =
    H.tr $ do H.td ! A.style "text-align: right"
                   $ label $ H.string (rowlabel ++ ":")
              H.td contents

prefTable :: Html
prefTable =
    H.table $ do
        row "Username" $ input ! type_ "input"
                               ! name  "username"
        row "Email" $ input ! type_ "input"
                            ! name  "email"
        forM_ prefMap prefRow

  where
      prefRow (v, s) =
          row s $ do "hate"
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

             username <- look "username"
             email    <- look "email"
             formData <- sequence $ map (runText . fst) prefMap
             let prefs = buildPref formData

             liftIO $ update db (SetPrefs (username, email) prefs)

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

      runText idx = do formVal <- optional $ look idx
                       let rating = fmap read formVal
                       return (idx, fromMaybe 2 rating)

      printScores pref game =
          H.p $ do
              toHtml $ gameName game
              H.br
              toHtml (show . score pref $ metadata game)

servePrefs :: App Response
servePrefs =
    do method GET
       requests <- fmap (splitOn ":") <$> looks "requests"
       let reqPairs = liftM2 (,) (!! 0) (!! 1) <$> filter ((2 ==) . length) requests
       db <- ask
       prefs <- liftIO $ query db (GetPrefs reqPairs)
       ok . serveCSV . export $ prefs

