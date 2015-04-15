{-# LANGUAGE DeriveDataTypeable #-}
module Haystack.User where

import Data.List ((\\), sortBy)
import Data.Ord (comparing)
import Data.Typeable
import Haystack.Game
import Utils (unwrapPair)

import CSV (CSV, OfUsers, OfOwner, OfPrefs, rowWhere, labels, column, asBool, asInt)


data User = User { username :: String
                 , prefs :: GamePref
                 , owned :: [String] } deriving (Eq, Show, Typeable)

recommended :: [Game] -> User -> [(Game, Int)]
recommended games u = sortBy (flip $ comparing snd)
                    . scoreGames (prefs u)
                    $ filter (flip notElem (owned u) . gameName) games

getUserRow :: CSV a -> String -> Maybe [String]
getUserRow csv name = rowWhere csv "username" (name ==)

getUser :: CSV OfUsers -> CSV OfPrefs -> CSV OfOwner -> String -> Maybe User
getUser ucsv pcsv ocsv name =
    do popular  <- pget asInt "popular"
       gems     <- pget asInt "gems"
       new      <- pget asInt "new"
       family   <- pget asInt "family"
       party    <- pget asInt "party"
       abstract <- pget asInt "abstract"
       strategy <- pget asInt "strategy"
       player2  <- pget asInt "player2"
       player3  <- pget asInt "player3"
       owned    <- getOwnership ocsv name
       return User { username = name
                   , owned = owned
                   , prefs = GamePref { likesPopular    = popular
                                      , likesNewRelease = new
                                      , likesForgotten  = gems
                                      , likesFamily     = family
                                      , likesParty      = party
                                      , likesAbstract   = abstract
                                      , likesStrategy   = strategy
                                      , likes2Player    = player2
                                      , likes3Player    = player3
                                      } }
  where uget f col = do row <- getUserRow ucsv name
                        column ucsv col f row
        pget f col = do row <- getUserRow pcsv name
                        column pcsv col f row

-- this is gnarly, but hey it works
getOwnership :: CSV OfOwner -> String -> Maybe [String]
getOwnership csv name = do pairs <- sequence . map owns $ labels csv
                           return . map fst $ filter snd pairs
  where owns label = do row <- getUserRow csv name
                        unwrapPair . ((,) label)
                                   . column csv label asBool
                                   $ row
