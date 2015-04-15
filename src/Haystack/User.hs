{-# LANGUAGE DeriveDataTypeable #-}
module Haystack.User where

import Data.List ((\\))
import Data.Typeable
import Haystack.Game
import Utils (unwrapPair)

import CSV (CSV, OfUsers, OfOwner, rowWhere, labels, column, asBool)


data User = User { username :: String
                 , prefs :: GamePref
                 , owned :: [Game] } deriving (Eq, Show, Typeable)

recommended :: [Game] -> User -> [(Game, Int)]
recommended games u = scoreGames (prefs u)
                    $ games \\ (owned u)

getUserRow :: CSV a -> String -> Maybe [String]
getUserRow csv name = rowWhere csv "username" (name ==)

getUser :: CSV OfUsers -> String -> Maybe [String]
getUser = getUserRow

-- this is gnarly, but hey it works
getOwnership :: CSV OfOwner -> String -> Maybe [String]
getOwnership csv name = do pairs <- sequence . map owns $ labels csv
                           return . map fst $ filter snd pairs
  where owns label = do row <- getUserRow csv name
                        unwrapPair . ((,) label)
                                   . column csv label asBool
                                   $ row
