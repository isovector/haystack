{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Haystack.Game where

import Control.Monad (liftM2)
import Data.Foldable (foldlM)
import Data.Typeable
import Safe

import Utils         (pmap)
import CSV           (CSV, OfGames, OfPrefs, asInt, asBool, asString, rows, column, toCSV)

import Haystack.Types


data Game = Game { gameName :: String
                 , inventory :: Int
                 , metadata :: GameData
                 } deriving (Eq, Show, Read, Typeable)

data Category = Popular | NewRelease | Forgotten deriving (Eq, Show, Read, Typeable)
data GameData = GameData { category :: Category
                         , isFamily :: Bool
                         , isParty :: Bool
                         , isAbstract :: Bool
                         , isStrategy :: Bool
                         , is2Player :: Bool
                         , is3Player :: Bool
                         } deriving (Eq, Show, Read, Typeable)

data GamePref = GamePref { likesPopular :: Int
                         , likesNewRelease :: Int
                         , likesForgotten :: Int
                         , likesFamily :: Int
                         , likesParty :: Int
                         , likesAbstract :: Int
                         , likesStrategy :: Int
                         , likes2Player :: Int
                         , likes3Player :: Int
                         } deriving (Eq, Show, Read, Typeable)


export :: [((String, String), GamePref)] -> CSV OfPrefs
export ps = toCSV [ "username"
                  , "popular"
                  , "gems"
                  , "new"
                  , "family"
                  , "party"
                  , "abstract"
                  , "strategy"
                  , "player2"
                  , "player3"
                  ]
                  $ flip map ps (
                      \((u,_),p) -> u : map (show . ($ p))
                                      [ likesPopular
                                      , likesNewRelease
                                      , likesNewRelease
                                      , likesForgotten
                                      , likesFamily
                                      , likesParty
                                      , likesAbstract
                                      , likesStrategy
                                      , likes2Player
                                      , likes3Player
                                      ])



csvToGames :: CSV OfGames -> [Game]
csvToGames csv = rights . map toGame $ rows csv
  where
      toGame row = do name      <- get asString "name"
                      inventory <- get asInt "inventory"
                      category  <- get asCategory "category"
                      family    <- get asBool "family"
                      party     <- get asBool "party"
                      abstract  <- get asBool "abstract"
                      strategy  <- get asBool "strategy"
                      player2   <- get asBool "player2"
                      player3   <- get asBool "player3"
                      return Game { gameName = name
                                  , inventory = inventory
                                  , metadata = GameData { category   = category
                                                        , isFamily   = family
                                                        , isParty    = party
                                                        , isAbstract = abstract
                                                        , isStrategy = strategy
                                                        , is2Player  = player2
                                                        , is3Player  = player3
                                                        }}
        where get f col = column csv col f row


score :: GamePref -> GameData -> Int
score pref game =
    foldl1 (+) $
      zipWith aspectScore
          [ likesPopular
          , likesNewRelease
          , likesForgotten
          , likesFamily
          , likesParty
          , likesAbstract
          , likesStrategy
          , likes2Player
          , likes3Player
          ]
          [ isCategory Popular
          , isCategory NewRelease
          , isCategory Forgotten
          , isFamily
          , isParty
          , isAbstract
          , isStrategy
          , is2Player
          , is3Player
          ]
  where isCategory c g = c == category g
        aspectScore f b = let x = f pref
                           in if b game then x
                                   else  4 - x

scoreGames :: GamePref -> [Game] -> [(Game, Int)]
scoreGames pref games = pmap (id, score pref . metadata)
                      $ zip games games

asCategory :: String -> Try Category
asCategory mc = case readMay mc of
                  Just c  -> return c
                  Nothing -> throwError $
                    flip (BadParse mc) "games.csv" (
                        "category field must be one of: "
                            ++ show "Popular"
                            ++ ", "
                            ++ show "NewRelease"
                            ++ ", "
                            ++ show "Forgotten" )

