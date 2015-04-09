{-# LANGUAGE RankNTypes #-}

module Haystack.Game where

import Control.Monad (liftM2)
import Data.Foldable (foldlM)
import Data.Maybe (catMaybes)
import Utils (pmap, partialLift)


data Game = Game { name :: String
                 , metadata :: GameData
                 } deriving (Eq, Show, Read)

data Category = Popular | NewRelease | Forgotten deriving (Eq, Show, Read)
data GameData = GameData { category :: Maybe Category
                         , isFamily :: Maybe Bool
                         , isParty :: Maybe Bool
                         , isAbstract :: Maybe Bool
                         , isStrategy :: Maybe Bool
                         , is2Player :: Maybe Bool
                         , is3Player :: Maybe Bool
                         } deriving (Eq, Show, Read)

type GamePref = GameData

over :: (forall b. Eq b => Maybe b -> Maybe b -> a)
     -> GamePref
     -> GameData
     -> [a]
over scoreBy pref game = subs
  where check f  = scoreBy (f pref) (f game)
        subs = [ check category
               , check isFamily
               , check isParty
               , check isAbstract
               , check isStrategy
               , check is2Player
               , check is3Player
               ]

score :: GamePref -> GameData -> Maybe Int
score pref game = foldlM (partialLift (+)) 0 $ over reduce pref game
  where reduce (Just a) (Just b) = Just $ if a == b then 1 else 0
        reduce _ _ = Just 0


scoreGames :: GamePref -> [Game] -> [(Game, Int)]
scoreGames pref games = catPairs
                      . pmap (id, score pref . metadata)
                      $ zip games games
  where catPairs :: [(a, Maybe b)] -> [(a, b)]
        catPairs = catMaybes . map extract

        extract :: (a, Maybe b) -> Maybe (a, b)
        extract (a, Just b)  = Just (a, b)
        extract (_, Nothing) = Nothing
