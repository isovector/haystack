module Haystack.User where

import Haystack.Game


data User = User { username :: String
                 , mustBe :: GamePref
                 , prefs :: GamePref
                 , owned :: [Game] }

