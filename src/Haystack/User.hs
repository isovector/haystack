{-# LANGUAGE DeriveDataTypeable #-}
module Haystack.User where

import Data.List ((\\))
import Data.Typeable
import Haystack.Game


data User = User { username :: String
                 , mustBe :: GamePref
                 , prefs :: GamePref
                 , owned :: [Game] } deriving (Eq, Show, Typeable)

{-
recommended :: [Game] -> User -> [(Game, Int)]
recommended games u = scoreGames (prefs u)
                    . filter (filtered (mustBe u) . metadata)
                    $ games \\ (owned u)
                    -}
