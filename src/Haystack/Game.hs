{-# LANGUAGE DeriveDataTypeable #-}
module Haystack.Game where

import Control.Monad (liftM2)
import Data.Foldable (foldlM)
import Data.Maybe    (catMaybes)
import Data.Typeable
import Utils         (pmap, partialLift)


data Game = Game { name :: String
                 , metadata :: GameData
                 } deriving (Eq, Show, Read, Typeable)

data Category = Popular | NewRelease | Forgotten deriving (Eq, Show, Read, Typeable)
data GameData = GameData { category :: Maybe Category
                         , isFamily :: Maybe Bool
                         , isParty :: Maybe Bool
                         , isAbstract :: Maybe Bool
                         , isStrategy :: Maybe Bool
                         , is2Player :: Maybe Bool
                         , is3Player :: Maybe Bool
                         } deriving (Eq, Show, Read, Typeable)

type GamePref = GameData

over :: a -> a -> a -> GamePref -> GameData -> [a]
over same diff missing pref game = subs
  where check f  = scoreBy (f pref) (f game)

        scoreBy (Just a) (Just b) = if a == b then same else diff
        scoreBy _ _ = missing

        subs = [ check category
               , check isFamily
               , check isParty
               , check isAbstract
               , check isStrategy
               , check is2Player
               , check is3Player
               ]

score :: GamePref -> GameData -> Int
score pref game = foldl1 (+) $ over 1 0 0 pref game

filtered :: GamePref -> GameData -> Bool
filtered pref game = foldl1 (&&) $ over True False True pref game


scoreGames :: GamePref -> [Game] -> [(Game, Int)]
scoreGames pref games = pmap (id, score pref . metadata)
                      $ zip games games

