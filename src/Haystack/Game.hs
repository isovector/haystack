module Haystack.Game where

import Control.Monad (liftM2)
import Data.Foldable (foldlM)
import Data.Maybe (catMaybes)
import Utils (pmap, partialLift)


data Game = Game { name :: String
                 , metadata :: GameData
                 } deriving (Eq, Show, Read)

data Category = Popular | NewRelease | ForgottenGem deriving (Eq, Show, Read)
data GameData = GameData { category :: Maybe Category
                         , isFamily :: Maybe Bool
                         , isParty :: Maybe Bool
                         , isAbstract :: Maybe Bool
                         , isStrategy :: Maybe Bool
                         , is2Player :: Maybe Bool
                         , is3Player :: Maybe Bool
                         } deriving (Eq, Show, Read)

type GamePref = GameData


score:: GamePref -> GameData -> Maybe Int
score pref game = foldlM (partialLift (+)) 0 subs
  where check :: Eq a => (GameData -> Maybe a) -> Maybe Int
        check f = reduce 1 (==) (f pref) (f game)

        reduce :: Int -> (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe Int
        reduce v f (Just a) (Just b) = if f a b then Just v else Nothing
        reduce _ _ _ _ = Just 0

        subs = [ check category
               , check isFamily
               , check isParty
               , check isAbstract
               , check isStrategy
               , check is2Player
               , check is3Player
               ]

scoreGames :: GamePref -> [Game] -> [(Game, Int)]
scoreGames pref games = catPairs
                      . pmap (id, score pref . metadata)
                      $ zip games games
  where catPairs :: [(a, Maybe b)] -> [(a, b)]
        catPairs = catMaybes . map extract

        extract :: (a, Maybe b) -> Maybe (a, b)
        extract (a, Just b)  = Just (a, b)
        extract (_, Nothing) = Nothing
