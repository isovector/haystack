{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Haystack.User where

import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import Data.List ((\\), sortBy)
import Data.Ord (comparing)
import Data.Typeable
import Haystack.Game
import Utils (unwrapPair)

import CSV (CSV, OfUsers, OfOwner, OfPrefs, rowWhere, labels, column, asBool, asInt, asString)
import Haystack.Types
import Haystack.Database

data ShipAddr = ShipAddr { shipName :: String
                         , company :: String
                         , ship1 :: String
                         , ship2 :: String
                         , city :: String
                         , state :: String
                         , postal :: String
                         , country :: String
                         , email :: String
                         } deriving (Eq, Typeable, Show)

data User = User { username :: String
                 , prefs :: GamePref
                 , address :: ShipAddr
                 , owned :: [String] } deriving (Eq, Typeable)

instance Show User where
    show (User { address = ShipAddr { .. }, .. }) =
        concat $ map ((++ ",") . show) [ "" -- order no.
                                       , shipName
                                       , company
                                       , ship1
                                       , ship2
                                       , city
                                       , state
                                       , postal
                                       , country
                                       , email
                                       , "" -- shipment
                                       ]


recommended :: [Game] -> User -> [(Game, Int)]
recommended games u = sortBy (flip $ comparing snd)
                    . scoreGames (prefs u)
                    $ filter (flip notElem (owned u) . gameName) games

getUserRow :: CSV a -> String -> Try [String]
getUserRow csv name = rowWhere csv "username" (name ==)

getUser :: CSV OfUsers
        -> CSV OfPrefs
        -> CSV OfOwner
        -> [Owned]
        -> String
        -> Try User
getUser ucsv pcsv ocsv owns name =
    do popular      <- pget asInt "popular"
       gems         <- pget asInt "gems"
       new          <- pget asInt "new"
       family       <- pget asInt "family"
       party        <- pget asInt "party"
       abstract     <- pget asInt "abstract"
       strategy     <- pget asInt "strategy"
       player2      <- pget asInt "player2"
       player3      <- pget asInt "player3"

       let myOwns = snd =<< filter ((== name) . fst) owns
       owned        <- (++ myOwns) <$> getOwnership ocsv name

       addrShipName <- uget asString "shipName"
       addrCompany  <- uget asString "company"
       addrShip1    <- uget asString "ship1"
       addrShip2    <- uget asString "ship2"
       addrCity     <- uget asString "city"
       addrState    <- uget asString "state"
       addrPostal   <- uget asString "postal"
       addrCountry  <- uget asString "country"
       addrEmail    <- uget asString "email"

       return User { username = name
                   , owned = owned
                   , address = ShipAddr { shipName = addrShipName
                                        , company  = addrCompany
                                        , ship1    = addrShip1
                                        , ship2    = addrShip2
                                        , city     = addrCity
                                        , state    = addrState
                                        , postal   = addrPostal
                                        , country  = addrCountry
                                        , email    = addrEmail
                                        }
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
        pget f col = do case getUserRow pcsv name of
                            Right row -> column pcsv col f row
                            Left _    -> return 2

-- this is gnarly, but hey it works
getOwnership :: CSV OfOwner -> String -> Try [String]
getOwnership csv name = do pairs <- sequence . map owns $ labels csv
                           return . map fst $ filter snd pairs
  where owns label = do row <- getUserRow csv name
                        unwrapPair . ((,) label)
                                   . column csv label asBool
                                   $ row
