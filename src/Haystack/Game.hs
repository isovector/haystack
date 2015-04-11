{-# LANGUAGE DeriveDataTypeable #-}
module Haystack.Game where

import Control.Monad (liftM2)
import Data.Foldable (foldlM)
import Data.Maybe    (catMaybes)
import Data.Typeable
import Utils         (pmap, partialLift)


data Game = Game { gameName :: String
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

