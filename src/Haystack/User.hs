{-# LANGUAGE DeriveDataTypeable #-}
module Haystack.User where

import Data.List ((\\))
import Data.Typeable
import Haystack.Game

import CSV (CSV, OfUsers, rowWhere)


data User = User { username :: String
                 , prefs :: GamePref
                 , owned :: [Game] } deriving (Eq, Show, Typeable)

recommended :: [Game] -> User -> [(Game, Int)]
recommended games u = scoreGames (prefs u)
                    $ games \\ (owned u)

getUser :: CSV OfUsers -> String -> Maybe [String]
getUser csv name = rowWhere csv "username" (name ==)

