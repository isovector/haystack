module Haystack.User where

import Haystack.Game


data User = User { username :: String
                 , prefs :: GamePref
                 , owned :: [Game] }


